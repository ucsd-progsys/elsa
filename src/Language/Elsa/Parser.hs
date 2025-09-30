{-# LANGUAGE FlexibleInstances #-}

module Language.Elsa.Parser
  ( parse
  , parseFile
  ) where

import qualified Control.Exception          as Ex
import           Control.Monad          (void)
import           Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char
import           Text.Megaparsec.Stream ()
import qualified Data.List as L
import           Language.Elsa.Types
import           Language.Elsa.UX
import           Data.List.NonEmpty         as NE

type Parser = Parsec SourcePos Text

--------------------------------------------------------------------------------
parse :: FilePath -> Text -> SElsa
--------------------------------------------------------------------------------
parse = parseWith elsa

parseWith  :: Parser a -> FilePath -> Text -> a
parseWith p f s = case runParser (whole p) f s of
                    Left pErrs -> Ex.throw (mkErrors pErrs f s) -- panic (show err) (posSpan . NE.head . errorPos $ err)
                    Right e  -> e

mkErrors :: ParseErrorBundle Text SourcePos -> FilePath -> Text -> [UserError]
mkErrors b f s = [ mkError (parseErrorPretty e) (span e) | e <- NE.toList (bundleErrors b)]
  where
    span e = let (l, c) = lineCol s (errorOffset e) in posSpan (SourcePos f (mkPos l) (mkPos c))

-- PosState looks relevant for finding line/column, but I (Justin) don't know how to use it

lineCol :: String -> Int -> (Int, Int)
lineCol s i = foldl f (1, 1) (Prelude.take i s)
  where
    f (l, c) char = if char == '\n' then (l + 1, 1) else (l, c + 1)

instance ShowErrorComponent SourcePos where
  showErrorComponent = show
 

-- panic msg sp = throw [Error msg sp]
-- instance Located (ParseError SourcePos Text) where
--  sourceSpan = posSpan . errorPos

-- instance PPrint (ParseError SourcePos Text) where
--   pprint = show

--------------------------------------------------------------------------------
parseFile :: FilePath -> IO SElsa
--------------------------------------------------------------------------------
parseFile f = parse f <$> readFile f

-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = sc *> p <* eof

-- RJ: rename me "space consumer"
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment  "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

-- | `symbol s` parses just the string s (and trailing whitespace)
symbol :: String -> Parser String
symbol = L.symbol sc

arrow :: Parser String
arrow = symbol "->"

colon :: Parser String
colon = symbol ":"

equal :: Parser String
equal = symbol "="

lam :: Parser String
lam = symbol "\\"


-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = betweenS "(" ")"

betweenS :: String -> String -> Parser a -> Parser a
betweenS l r = between (symbol l) (symbol r)

-- | `lexeme p` consume whitespace after running p
lexeme :: Parser a -> Parser (a, SourceSpan)
lexeme p = L.lexeme sc (withSpan p)

-- | `rWord`
rWord   :: String -> Parser SourceSpan
rWord w = snd <$> (withSpan (string w) <* notFollowedBy alphaNumChar <* sc)

-- | list of reserved words
keywords :: [Text]
keywords = [ "let"  , "eval"  , "conf" ]

-- | `identifier` parses identifiers: lower-case alphabets followed by alphas or digits
identifier :: Parser (String, SourceSpan)
identifier = lexeme (p >>= check)
  where
    p       = (:) <$> letterChar <*> many identChar -- alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

identChar :: Parser Char
identChar =  alphaNumChar
         <|> oneOf ['_', '#', '\'']

-- | `binder` parses BareBind, used for let-binds and function parameters.
binder :: Parser SBind
binder = uncurry Bind <$> identifier

withSpan' :: Parser (SourceSpan -> a) -> Parser a
withSpan' p = do
  p1 <- getSourcePos
  f  <- p
  p2 <- getSourcePos
  return (f (SS p1 p2))

withSpan :: Parser a -> Parser (a, SourceSpan)
withSpan p = do
  p1 <- getSourcePos
  x  <- p
  p2 <- getSourcePos
  return (x, SS p1 p2)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
elsa :: Parser SElsa
elsa = do
  items <- many elsaItem
  pure $
    Elsa
      { defns = [d | DefnItem d <- items],
        evals = [e | EvalItem e <- items]
      }

elsaItem :: Parser SElsaItem
elsaItem = 
  (DefnItem <$> defn) <|> (EvalItem <$> eval)

defn :: Parser SDefn
defn = do
  rWord "let"
  b <- binder <* equal
  e <- expr
  return (Defn b e)

eval :: Parser SEval
eval = do
  kind <- (rWord "eval" >> return Regular) <|> (rWord "conf" >> return Conf)
  name  <- binder
  colon
  root  <- expr
  steps <- many step
  return $ Eval kind name root steps

step :: Parser SStep
step = Step <$> eqn <*> expr

eqn :: Parser SEqn
eqn = withSpan' parseEqn

parseEqn :: Parser (SourceSpan -> Eqn SourceSpan)
parseEqn = try parseUnEqn <|> parseRegEqn

parseUnEqn :: Parser (SourceSpan -> Eqn SourceSpan)
parseUnEqn = do
  void $ char '<'
  op <- choice
    [ try (symbol "n*=") >> return EqUnNormOrdTrans
    , try (symbol "p*=") >> return EqUnAppOrdTrans
    , try (symbol "b=")  >> return EqUnBeta
    , try (symbol "n=") >> return EqUnNormOrd
    , try (symbol "p=") >> return EqUnAppOrd
    , try (symbol "e=")  >> return EqUnEta
    , try (symbol "*=")  >> return EqUnTrans
    ]
  return $ \sp -> Eqn op Nothing sp

parseRegEqn :: Parser (SourceSpan -> Eqn SourceSpan)
parseRegEqn = do
  void $ char '='
  op <- choice
    [ try (string "n*") >> return EqNormOrdTrans
    , try (string "p*") >> return EqAppOrdTrans
    , try (string "n") >> return EqNormOrd
    , try (string "p") >> return EqAppOrd
    , try (string "a")  >> return EqAlpha
    , try (string "b")  >> return EqBeta
    , try (string "e")  >> return EqEta
    , try (string "d")  >> return EqDefn
    , try (string "*")  >> return EqTrans
    , try (string "~")  >> return EqNormTrans
    ]
  mChk <- optional parseNormCheck
  void $ symbol ">"
  return $ \sp -> Eqn op mChk sp

parseNormCheck :: Parser NormCheck
parseNormCheck = do
  void $ char ':'
  choice
    [ char 's' >> return Strong
    , char 'w' >> return Weak
    , char 'h' >> return Head
    ]

expr :: Parser SExpr
expr =  try lamExpr
    <|> try appExpr
    <|> try idExpr
    <|> parenExpr

parenExpr :: Parser SExpr
parenExpr = parens expr

idExpr :: Parser SExpr
idExpr = uncurry EVar <$> identifier

appExpr :: Parser SExpr
appExpr  = apps <$> funExpr <*> sepBy1 funExpr sc
  where
    apps = L.foldl' (\e1 e2 -> EApp e1 e2 (tag e1 `mappend` tag e2))

funExpr :: Parser SExpr
funExpr = try idExpr <|> parenExpr

lamExpr :: Parser SExpr
lamExpr = do
  lam
  xs    <- sepBy binder sc <* arrow
  e     <- expr
  return (mkLam xs e)

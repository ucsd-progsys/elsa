module Language.Elsa.Parser ( parse, parseFile ) where

import           Control.Monad (void)
import           Text.Megaparsec hiding (parse)
import           Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Data.List as L
import           Language.Elsa.Types
import           Language.Elsa.UX

--------------------------------------------------------------------------------
parse :: FilePath -> Text -> SElsa
--------------------------------------------------------------------------------
parse = parseWith elsa

parseWith  :: Parser a -> FilePath -> Text -> a
parseWith p f s = case runParser (whole p) f s of
                    Left err -> panic (show err) (posSpan . errorPos $ err)
                    Right e  -> e

instance Located ParseError where
  sourceSpan = posSpan . errorPos

instance PPrint ParseError where
  pprint = show

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
keywords = [ "let"  , "eval" ]

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
         <|> oneOf "_#'"

-- | `binder` parses BareBind, used for let-binds and function parameters.
binder :: Parser SBind
binder = uncurry Bind <$> identifier

withSpan' :: Parser (SourceSpan -> a) -> Parser a
withSpan' p = do
  p1 <- getPosition
  f  <- p
  p2 <- getPosition
  return (f (SS p1 p2))

withSpan :: Parser a -> Parser (a, SourceSpan)
withSpan p = do
  p1 <- getPosition
  x  <- p
  p2 <- getPosition
  return (x, SS p1 p2)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
elsa :: Parser SElsa
elsa = Elsa <$> many defn <*> many eval

defn :: Parser SDefn
defn = do
  rWord "let"
  b <- binder <* equal
  e <- expr
  return (Defn b e)

eval :: Parser SEval
eval = do
  rWord "eval"
  name  <- binder
  colon
  root  <- expr
  steps <- many step
  return $ Eval name root steps

step :: Parser SStep
step = Step <$> eqn <*> expr

eqn :: Parser SEqn
eqn =  try (withSpan' (symbol "=a>" >> return AlphEq))
   <|> try (withSpan' (symbol "=b>" >> return BetaEq))
   <|> try (withSpan' (symbol "<b=" >> return UnBeta))
   <|> try (withSpan' (symbol "=d>" >> return DefnEq))
   <|>     (withSpan' (symbol "=*>" >> return TrnsEq))

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

-- | This module contains the code for all the user (programmer) facing
--   aspects, i.e. error messages, source-positions, overall results.

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Language.Elsa.UX
  (
  -- * Representation
    SourceSpan (..)
  , Located (..)

  -- * Usage Mode
  , Mode (..)

  -- * Extraction from Source file
  , readFileSpan

  -- * Constructing spans
  , posSpan, junkSpan

  -- * Success and Failure
  , UserError
  , eMsg
  , eSpan
  -- , Result

  -- * Throwing & Handling Errors
  , mkError
  , abort
  , panic
  , renderErrors

  -- * Pretty Printing
  , Text
  , PPrint (..)
  ) where

import           Control.Exception
import           Data.Typeable
import qualified Data.List as L
import           Text.Printf (printf)
import           Text.Megaparsec
import           Text.JSON hiding (Error)
import           Language.Elsa.Utils

type Text = String

class PPrint a where
  pprint :: a -> Text

--------------------------------------------------------------------------------
-- | Accessing SourceSpan
--------------------------------------------------------------------------------
class Located a where
  sourceSpan :: a -> SourceSpan

instance Located SourceSpan where
  sourceSpan x = x

--------------------------------------------------------------------------------
-- | Source Span Representation
--------------------------------------------------------------------------------
data SourceSpan = SS
  { ssBegin :: !SourcePos
  , ssEnd   :: !SourcePos
  }
  deriving (Eq, Show)

instance Semigroup SourceSpan where
  x <> y = mappendSpan x y 

instance Monoid SourceSpan where
  mempty  = junkSpan
  mappend = mappendSpan

mappendSpan :: SourceSpan -> SourceSpan -> SourceSpan
mappendSpan s1 s2
  | s1 == junkSpan = s2
  | s2 == junkSpan = s1
  | otherwise      = SS (ssBegin s1) (ssEnd s2)

instance PPrint SourceSpan where
  pprint = ppSourceSpan

ppSourceSpan :: SourceSpan -> String
ppSourceSpan s
  | l1 == l2  = printf "%s:%d:%d-%d"        f l1 c1 c2
  | otherwise = printf "%s:(%d:%d)-(%d:%d)" f l1 c1 l2 c2
  where
    (f, l1, c1, l2, c2) = spanInfo s

spanInfo :: SourceSpan -> (FilePath, Int, Int, Int, Int)
spanInfo s = (f s, l1 s, c1 s, l2 s, c2 s)
  where
    f      = spanFile
    l1     = unPos . sourceLine   . ssBegin
    c1     = unPos . sourceColumn . ssBegin
    l2     = unPos . sourceLine   . ssEnd
    c2     = unPos . sourceColumn . ssEnd

--------------------------------------------------------------------------------
-- | Source Span Extraction
--------------------------------------------------------------------------------
readFileSpan :: SourceSpan -> IO String
--------------------------------------------------------------------------------
readFileSpan sp = getSpan sp <$> readFile (spanFile sp)


spanFile :: SourceSpan -> FilePath
spanFile = sourceName . ssBegin

getSpan :: SourceSpan -> String -> String
getSpan sp
  | sameLine    = getSpanSingle l1 c1 c2
  | sameLineEnd = getSpanSingleEnd l1 c1
  | otherwise   = getSpanMulti  l1 l2
  where
    sameLine            = l1 == l2
    sameLineEnd         = l1 + 1 == l2 && c2 == 1
    (_, l1, c1, l2, c2) = spanInfo sp


getSpanSingleEnd :: Int -> Int -> String -> String
getSpanSingleEnd l c1
  = highlightEnd l c1
  . safeHead ""
  . getRange l l
  . lines

getSpanSingle :: Int -> Int -> Int -> String -> String
getSpanSingle l c1 c2
  = highlight l c1 c2
  . safeHead ""
  . getRange l l
  . lines

getSpanMulti :: Int -> Int -> String -> String
getSpanMulti l1 l2
  = highlights l1
  . getRange l1 l2
  . lines

highlight :: Int -> Int -> Int -> String -> String
highlight l c1 c2 s = unlines
  [ cursorLine l s
  , replicate (12 + c1) ' ' <> replicate (c2 - c1) '^'
  ]

highlightEnd :: Int -> Int -> String -> String
highlightEnd l c1 s = highlight l c1 (1 + length s') s'
  where
    s'              = trimEnd s

highlights :: Int -> [String] -> String
highlights i ls = unlines $ zipWith cursorLine [i..] ls

cursorLine :: Int -> String -> String
cursorLine l = printf "%s|  %s" (lineString l)

lineString :: Int -> String
lineString n = replicate (10 - nD) ' ' <> nS
  where
    nS       = show n
    nD       = length nS

--------------------------------------------------------------------------------
-- | Source Span Construction
--------------------------------------------------------------------------------
posSpan :: SourcePos -> SourceSpan
--------------------------------------------------------------------------------
posSpan p = SS p p

junkSpan :: SourceSpan
junkSpan = posSpan (initialPos "unknown")

--------------------------------------------------------------------------------
-- | Usage Mode
--------------------------------------------------------------------------------
data Mode
  = Json
  | Cmdline
  | Server
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Representing (unrecoverable) failures
--------------------------------------------------------------------------------
data UserError = Error
  { eMsg  :: !Text
  , eSpan :: !SourceSpan
  }
  deriving (Show, Typeable)

instance Located UserError where
  sourceSpan = eSpan

instance Exception [UserError]

--------------------------------------------------------------------------------
panic :: String -> SourceSpan -> a
--------------------------------------------------------------------------------
panic msg sp = throw [Error msg sp]

--------------------------------------------------------------------------------
abort :: UserError -> b
--------------------------------------------------------------------------------
abort e = throw [e]

--------------------------------------------------------------------------------
mkError :: Text -> SourceSpan -> UserError
--------------------------------------------------------------------------------
mkError = Error

--------------------------------------------------------------------------------
renderErrors :: Mode -> [UserError] -> IO Text
--------------------------------------------------------------------------------
renderErrors Json    es = return (renderErrorsJson es)
renderErrors Server  es = return (renderResultJson es)
renderErrors Cmdline es = renderErrorsText es

renderErrorsText :: [UserError] -> IO Text
renderErrorsText [] =
  return ""
renderErrorsText es = do
  errs  <- mapM renderError es
  return $ L.intercalate "\n" ("Errors found!" : errs)

renderError :: UserError -> IO Text
renderError e = do
  let sp   = sourceSpan e
  snippet <- readFileSpan sp
  return   $ printf "%s: %s\n\n%s" (pprint sp) (eMsg e) snippet

renderErrorsJson :: [UserError] -> Text
renderErrorsJson es = "RESULT\n" ++ showJSValue' (showJSON es)

showJSValue'   :: JSValue -> Text
showJSValue' x = showJSValue x ""

renderResultJson :: [UserError] -> Text
renderResultJson es = showJSValue' $ jObj
                    [ ("types"  , jObj []    )
                    , ("status" , status   es)
                    , ("errors" , showJSON es)
                    ]
  where
    status []       = showJSON ("safe"   :: String)
    status _        = showJSON ("unsafe" :: String)


instance JSON UserError where
  readJSON     = undefined
  showJSON err = jObj [ ("start"  , showJSON $ start err)
                      , ("stop"   , showJSON $ stop err )
                      , ("message", showJSON $ eMsg err )
                      ]
    where
      start    = ssBegin . eSpan
      stop     = ssEnd   . eSpan

jObj = JSObject . toJSObject

instance JSON SourcePos where
  readJSON    = undefined
  showJSON sp = jObj [ ("line"  , showJSON (unPos l))
                     , ("column", showJSON (unPos c))
                     ]
    where
      l       = sourceLine   sp
      c       = sourceColumn sp

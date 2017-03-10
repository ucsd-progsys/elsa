{-# LANGUAGE ScopedTypeVariables #-}

module Language.Elsa.Runner where

import Data.List            (intercalate)
import Data.Maybe           (mapMaybe)
import Control.Monad        (unless)
import Control.Exception
import System.IO
import System.Exit
import System.Environment   (getArgs)
import Language.Elsa.Parser (parse)
import Language.Elsa.Types  (successes, resultError)
import Language.Elsa.UX
import Language.Elsa.Eval   (elsa)


topMain:: IO ()
topMain = do
  (js,f) <- getSrcFile
  s      <- readFile f
  runElsa js f s `catch` exitErrors js

exitErrors :: Bool -> [UserError] -> IO ()
exitErrors json = esHandle json stderr exitFailure

esHandle :: Bool -> Handle -> IO a -> [UserError] -> IO a
esHandle json h exitF es = renderErrors json es >>= hPutStrLn h >> exitF

runElsa :: Bool -> FilePath -> Text -> IO ()
runElsa json f s = do
  let rs  = elsa (parse f s)
  let es  = mapMaybe resultError rs
  case es of
    [] -> unless json (putStrLn (okMessage rs)) >> exitSuccess
    _  -> exitErrors json es

okMessage rs = "OK " ++ intercalate ", " (successes rs) ++ "."

getSrcFile :: IO (Bool, Text)
getSrcFile = do
  args <- getArgs
  case args of
    ["--json", f] -> return (True,  f)
    [f]           -> return (False, f)
    _             -> error "Please run with a single file as input"

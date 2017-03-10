{-# LANGUAGE ScopedTypeVariables #-}

module Language.Elsa.Runner where

import Data.Maybe (mapMaybe)
import Control.Exception
import System.IO
import System.Exit
import System.Environment   (getArgs)
import Language.Elsa.Parser (parse)
import Language.Elsa.Types  (resultError)
import Language.Elsa.UX
import Language.Elsa.Eval   (elsa)

topMain:: IO ()
topMain = runElsa `catch` exitErrors

exitErrors :: [UserError] -> IO ()
exitErrors = esHandle stderr exitFailure

esHandle :: Handle -> IO a -> [UserError] -> IO a
esHandle h exitF es = renderErrors es >>= hPutStrLn h >> exitF

runElsa :: IO ()
runElsa = do
  f     <- getSrcFile
  s     <- readFile f
  let rs = elsa (parse f s)
  let es = mapMaybe resultError rs
  case es of
    [] -> exitSuccess
    _  -> exitErrors es

getSrcFile :: IO Text
getSrcFile = do
  args <- getArgs
  case args of
    [f] -> return f
    _   -> error "Please run with a single file as input"

{-# LANGUAGE ScopedTypeVariables #-}

module Language.Elsa.Runner where

import Data.List            (intercalate)
import Data.Maybe           (mapMaybe)
import Control.Monad        (when)
import Control.Exception
import System.IO
import System.Exit
import System.Environment   (getArgs)
import System.FilePath      -- (takeDirectory, takeFileName)
import System.Directory     -- (takeDirectory, takeFileName)
import Language.Elsa.Parser (parse)
import Language.Elsa.Types  (successes, resultError)
import Language.Elsa.UX
import Language.Elsa.Eval   (elsa)


topMain:: IO ()
topMain = do
  (m, f) <- getSrcFile
  s      <- readFile f
  runElsa m f s `catch` exitErrors m f

exitErrors :: Mode -> FilePath -> [UserError] -> IO ()
exitErrors mode f es = esHandle mode (modeWriter mode f) resultExit es

resultExit :: [UserError] -> IO a
resultExit [] = exitSuccess
resultExit _  = exitFailure

esHandle :: Mode -> (Text -> IO ()) -> ([UserError] -> IO a) -> [UserError] -> IO a
esHandle mode writer exitF es = renderErrors mode es >>= writer >> exitF es

modeWriter :: Mode -> FilePath -> Text -> IO ()
modeWriter Cmdline _ s = hPutStrLn stderr s
modeWriter Json    _ s = hPutStrLn stderr s
modeWriter Server  f s = do createDirectoryIfMissing True jsonDir
                            writeFile jsonFile s
                            hPutStrLn stderr s
                         where
                            jsonDir  = takeDirectory f </> ".elsa"
                            jsonFile = jsonDir </> addExtension (takeFileName f) ".json"

runElsa :: Mode -> FilePath -> Text -> IO ()
runElsa mode f s = do
  let rs  = elsa (parse f s)
  let es  = mapMaybe resultError rs
  when (null es && mode == Cmdline) (putStrLn (okMessage rs))
  exitErrors mode f es

okMessage rs = "OK " ++ intercalate ", " (successes rs) ++ "."

getSrcFile :: IO (Mode, Text)
getSrcFile = do
  args <- getArgs
  case args of
    ["--json"  , f] -> return (Json,    f)
    ["--server", f] -> return (Server,  f)
    [f]             -> return (Cmdline, f)
    _               -> error "Please run with a single file as input"

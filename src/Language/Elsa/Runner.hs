{-# LANGUAGE ScopedTypeVariables #-}

module Language.Elsa.Runner
  ( topMain
  , runElsa
  , runElsaId
  ) where

import Data.List            (intercalate)
import Data.Maybe           (mapMaybe)
import Control.Monad        (when, void)
import Control.Exception
import System.IO
import System.Exit
import System.Environment   (getArgs)
import System.FilePath
import System.Directory
import System.Timeout
import Language.Elsa.Parser
import Language.Elsa.Types
import Language.Elsa.UX
import Language.Elsa.Eval
import qualified Language.Elsa.Utils as Utils

topMain:: IO ()
topMain = do
  (m, f) <- getSrcFile
  s      <- readFile f
  res    <- timeout (timeLimit * 10 ^ 6) (runElsa m f s `catch` exitErrors m f)
  case res of
    Just z  -> return z
    Nothing -> putStrLn timeMsg >> exitFailure

timeLimit :: Int
timeLimit = 10

timeMsg :: String
timeMsg = "Timed out after " ++ show timeLimit ++ " seconds."

getSrcFile :: IO (Mode, Text)
getSrcFile = do
  args <- getArgs
  case args of
    ["--json"  , f] -> return (Json,    f)
    ["--server", f] -> return (Server,  f)
    [f]             -> return (Cmdline, f)
    _               -> error "Please run with a single file as input"

exitErrors :: Mode -> FilePath -> [UserError] -> IO ()
exitErrors mode f es = esHandle mode (modeWriter mode f) resultExit es

resultExit :: [UserError] -> IO a
resultExit [] = say Utils.Happy >> exitSuccess
resultExit _  = say Utils.Sad   >> exitFailure

say :: Utils.Mood -> IO () 
say m = Utils.colorStrLn m (Utils.wrapStars (msg m))
  where 
    msg Utils.Happy = "OK"
    msg Utils.Sad   = "Errors found!"


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


---------------------------------------------------------------------------------------------------------
runElsa :: Mode -> FilePath -> Text -> IO ()
---------------------------------------------------------------------------------------------------------
runElsa mode f s = do
  let rs = elsa (parse f s)
  let es = mapMaybe resultError rs
  when (null es && mode == Cmdline) (putStrLn (okMessage rs))
  exitErrors mode f es

okMessage rs = "OK " ++ intercalate ", " (successes rs) ++ "."

--------------------------------------------------------------------------------
runElsaId :: FilePath -> Id -> IO (Maybe (Result ()))
--------------------------------------------------------------------------------
runElsaId f x = ((`runElsa1` x) <$> parseFile f)
                  `catch`
                     (\(_ :: [UserError]) -> return Nothing)

runElsa1 :: Elsa a -> Id -> Maybe (Result ())
runElsa1 p x = case elsaOn (== x) p of
                 [r] -> Just (void r)
                 _   -> Nothing

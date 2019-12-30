module Language.Elsa.Utils where

import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import qualified Data.Dequeue        as Q
import           Data.Hashable
import           Data.Char (isSpace)
import           Control.Exception
import           Text.Printf
import           System.Directory
import           System.FilePath
import           Debug.Trace (trace)
import           System.Console.ANSI


groupBy :: (Eq k, Hashable k) => (a -> k) -> [a] -> [(k, [a])]
groupBy f = M.toList . L.foldl' (\m x -> inserts (f x) x m) M.empty

inserts :: (Eq k, Hashable k) => k -> v -> M.HashMap k [v] -> M.HashMap k [v]
inserts k v m = M.insert k (v : M.lookupDefault [] k m) m

dupBy :: (Eq k, Hashable k) => (a -> k) -> [a] -> [[a]]
dupBy f xs = [ xs' | (_, xs') <- groupBy f xs, 2 <= length xs' ]

trim :: String -> String
trim = f . f  where f = reverse . dropWhile isSpace

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

ensurePath :: FilePath -> IO ()
ensurePath = createDirectoryIfMissing True . takeDirectory

safeReadFile :: FilePath -> IO (Either String String)
safeReadFile f = (Right <$> readFile f) `catch` handleIO f

handleIO :: FilePath -> IOException -> IO (Either String a)
handleIO f e = return . Left $ "Warning: Couldn't open " <> f <> ": " <> show e

traceShow :: (Show a) => String -> a -> a
traceShow msg x
  | False
  = trace (printf "TRACE: %s = %s" msg (show x)) x
  | otherwise
  = x

safeHead :: a -> [a] -> a
safeHead def []    = def
safeHead _   (x:_) = x

getRange :: Int -> Int -> [a] -> [a]
getRange i1 i2
  = take (i2 - i1 + 1)
  . drop (i1 - 1)

fromEither :: Either a a -> a
fromEither (Left x)  = x
fromEither (Right x) = x

--------------------------------------------------------------------------------
-- | Queue ---------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype Queue a = Q (Q.BankersDequeue a)

qEmpty :: Queue a
qEmpty = Q Q.empty

qInit :: a -> Queue a
qInit x = qPushes qEmpty [x]

qPushes :: Queue a -> [a] -> Queue a
qPushes (Q q) xs = Q (L.foldl' Q.pushFront q xs)

qPop :: Queue a -> Maybe (a, Queue a)
qPop (Q q) = case Q.popBack q of
               Nothing      -> Nothing
               Just (x, q') -> Just (x, Q q')


data Mood = Happy | Sad 

moodColor :: Mood -> Color
moodColor Sad   = Red 
moodColor Happy = Green

wrapStars :: String -> String
wrapStars msg = "\n**** " ++ msg ++ " " ++ replicate (74 - length msg) '*'

withColor :: Color -> IO () -> IO ()
withColor c act = do 
  setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
  act
  setSGR [ Reset]

colorStrLn :: Mood -> String -> IO ()
colorStrLn c = withColor (moodColor c) . putStrLn
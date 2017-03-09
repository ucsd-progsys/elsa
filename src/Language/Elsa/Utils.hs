module Language.Elsa.Utils where

import qualified Data.Map  as M
import qualified Data.List as L
import           Data.Monoid
import           Data.Char (isSpace)
import           Control.Exception
import           Text.Printf
import           System.Directory
import           System.FilePath
import           Debug.Trace (trace)

groupBy :: (Ord k) => (a -> k) -> [a] -> [(k, [a])]
groupBy f = M.toList . L.foldl' (\m x -> inserts (f x) x m) M.empty

inserts :: (Ord k) => k -> v -> M.Map k [v] -> M.Map k [v]
inserts k v m = M.insert k (v : M.findWithDefault [] k m) m

dupBy :: (Ord k) => (a -> k) -> [a] -> [[a]]
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
traceShow msg x = trace (printf "TRACE: %s = %s" msg (show x)) x

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

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import System.Directory
import System.Exit
import System.FilePath
import System.Environment
import System.IO
import System.IO.Error
import Control.Monad (when)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf
import Language.Elsa

main :: IO ()
main = defaultMain =<< group "Tests" [unitTests]

unitTests = group "Unit"
  [ testGroup "ok"      <$> dirTests "tests/ok"       TestOk
  , testGroup "further" <$> dirTests "tests/further"  TestPartial
  , testGroup "invalid" <$> dirTests "tests/invalid"  TestInvalid
  ]

data Outcome
  = TestOk
  | TestPartial
  | TestInvalid
  | TestMixed
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
dirTests :: FilePath -> Outcome -> IO [TestTree]
--------------------------------------------------------------------------------
dirTests root code
  = do files    <- walkDirectory root
       let tests = [ root </> rel | f <- files, isTest f, let rel = makeRelative root f ]
       return    $ mkTest code <$> tests

isTest   :: FilePath -> Bool
isTest f = takeExtension f `elem` [".lc"]

--------------------------------------------------------------------------------
mkTest :: Outcome -> FilePath -> TestTree
--------------------------------------------------------------------------------
mkTest code file
  = testCase file $ do
     res <- doTest file
     when (res /= code) (assertFailure "Wrong Result")

doTest :: FilePath -> IO Outcome
doTest f =  resultOutcome . elsa <$> parseFile f

resultOutcome :: [Result a] -> Outcome
resultOutcome rs = case (oks, invs, parts) of
                     (True, False, False) -> TestOk
                     (False, True, False) -> TestInvalid
                     (False, False, True) -> TestPartial
                     _                    -> TestMixed
  where
    oks          = notNull [ r | r@(OK {})      <- rs ]
    invs         = notNull [ r | r@(Invalid {}) <- rs ]
    parts        = notNull [ r | r@(Partial {}) <- rs ]
    notNull      = not . null

----------------------------------------------------------------------------------------
-- Generic Helpers
----------------------------------------------------------------------------------------

group n xs = testGroup n <$> sequence xs

----------------------------------------------------------------------------------------
walkDirectory :: FilePath -> IO [FilePath]
----------------------------------------------------------------------------------------
walkDirectory root
  = do (ds,fs) <- partitionM doesDirectoryExist . candidates =<< (getDirectoryContents root `catchIOError` const (return []))
       (fs++) <$> concatMapM walkDirectory ds
  where
    candidates fs = [root </> f | f <- fs, not (isExtSeparator (head f))]

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f = go [] []
  where
    go ls rs []     = return (ls,rs)
    go ls rs (x:xs) = do b <- f x
                         if b then go (x:ls) rs xs
                              else go ls (x:rs) xs

-- isDirectory :: FilePath -> IO Bool
-- isDirectory = fmap Posix.isDirectory . Posix.getFileStatus

concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ []     = pure []
concatMapM f (x:xs) = (++) <$> f x <*> concatMapM f xs

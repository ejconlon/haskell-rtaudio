module Main (main) where

import Control.Monad (when)
import Data.Foldable (for_)
import Sound.RtAudio
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

assertNonEmpty :: String -> [a] -> IO ()
assertNonEmpty thing lst = when (null lst) (assertFailure (thing ++ " empty"))

assertPositive :: (Num a, Ord a, Show a) => String -> a -> IO ()
assertPositive thing val = when (val < 0) (assertFailure (thing ++ " <= 0 (" ++ show val ++ ")"))

testGetVersion :: TestTree
testGetVersion = testCase "getVersion" $ do
  actualVersion <- getVersion
  actualVersion @?= "5.1.0"

testGetCompiledApis :: TestTree
testGetCompiledApis = testCase "getCompiledApis" $ do
  compiledApis <- getCompiledApis
  let numApis = length compiledApis
  assertPositive "num compiled apis" numApis

testApiName :: TestTree
testApiName = testCase "apiName" $ do
  actualName1 <- apiName UnspecifiedApi
  actualName1 @?= "unspecified"
  actualName2 <- apiName DummyApi
  actualName2 @?= "dummy"

testApiDisplayName :: TestTree
testApiDisplayName = testCase "apiDisplayName" $ do
  actualName1 <- apiDisplayName UnspecifiedApi
  actualName1 @?= "Unknown"
  actualName2 <- apiDisplayName DummyApi
  actualName2 @?= "Dummy"

testApiByName :: TestTree
testApiByName = testCase "apiByName" $ do
  actualApi1 <- apiByName "unspecified"
  actualApi1 @?= UnspecifiedApi
  actualApi2 <- apiByName "dummy"
  -- TODO(ejconlon) This... varies???
  -- dummy should get unspecified api
  -- actualApi2 @?= UnspecifiedApi
  actualApi3 <- apiByName "invalid"
  -- and an invalid name should also get unspecific api
  actualApi3 @?= UnspecifiedApi

testAudio :: TestTree
testAudio = testCase "audio" $ do
  apis <- getCompiledApis
  let expectedApi = head apis
  audio <- createAudio expectedApi
  actualApi <- audioCurrentApi audio
  actualApi @?= expectedApi
  deviceCount <- audioDeviceCount audio
  assertPositive "device count" deviceCount
  for_ [0 .. deviceCount - 1] $ \index -> do
    info <- audioGetDeviceInfo audio index
    assertNonEmpty "device info name" (diName info)

main :: IO ()
main = defaultMain $ testGroup "RtAudio"
  [ testGetVersion
  , testGetCompiledApis
  , testApiName
  , testApiDisplayName
  , testApiByName
  , testAudio
  ]

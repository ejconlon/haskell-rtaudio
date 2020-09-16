module Main (main) where

import Control.Monad (when)
import Data.Foldable (for_)
import Sound.RtAudio
import Sound.RtAudio.Report
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

assertNonEmpty :: String -> [a] -> IO ()
assertNonEmpty thing lst = when (null lst) (assertFailure (thing ++ " empty"))

assertPositive :: (Num a, Ord a, Show a) => String -> a -> IO ()
assertPositive thing val = when (val < 0) (assertFailure (thing ++ " <= 0 (" ++ show val ++ ")"))

assertInRange :: (Ord a, Show a) => String -> a -> a -> a -> IO ()
assertInRange thing lo hi val
  | val < lo = assertFailure (thing ++ " less than min bound (" ++ show val ++ " < " ++ show lo ++ ")")
  | val > hi = assertFailure (thing ++ " greater than max bound (" ++ show val ++ " > " ++ show hi ++ ")")
  | otherwise = pure ()

assertMember :: (Eq a, Show a) => String -> String -> a -> [a] -> IO ()
assertMember thingVal thingLst val lst
  | elem val lst = pure ()
  | otherwise = assertFailure (thingVal ++ " " ++ show val ++ " is not member of " ++ thingLst ++ " " ++ show lst)

expectedVersion :: String
expectedVersion = "5.1.0"

testGetVersion :: TestTree
testGetVersion = testCase "getVersion" $ do
  actualVersion <- getVersion
  actualVersion @?= expectedVersion

testGetCompiledApis :: TestTree
testGetCompiledApis = testCase "getCompiledApis" $ do
  compiledApis <- getCompiledApis
  assertNonEmpty "compiled apis" compiledApis

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
  actualApi <- currentApi audio
  actualApi @?= expectedApi
  devCount <- deviceCount audio
  assertPositive "device count" devCount
  for_ [0 .. devCount - 1] $ \index -> do
    info <- getDeviceInfo audio index
    assertNonEmpty "device info name" (diName info)
    assertMember "preferred SR" "supportedSRs" (diPreferredSampleRate info) (fmap fromIntegral (diSampleRates info))
  defOut <- getDefaultOutputDevice audio
  assertInRange "default output" 0 (devCount - 1) defOut
  defIn <- getDefaultInputDevice audio
  assertInRange "default input" 0 (devCount - 1) defIn

testReport :: TestTree
testReport = testCase "report" $ do
  -- We could test more but we mostly just want to make sure we can run 'buildReport'.
  -- The rest is covered by 'testAudio'.
  Report actualVersion actualApiReports <- buildReport
  actualVersion @?= expectedVersion

main :: IO ()
main = defaultMain $ testGroup "RtAudio"
  [ testGetVersion
  , testGetCompiledApis
  , testApiName
  , testApiDisplayName
  , testApiByName
  , testAudio
  , testReport
  ]

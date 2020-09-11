module Main (main) where

import Control.Monad (unless)
import Sound.RtAudio
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

testGetVersion :: TestTree
testGetVersion = testCase "getVersion" $ do
  actualVersion <- getVersion
  actualVersion @?= "5.1.0"

testGetCompiledApis :: TestTree
testGetCompiledApis = testCase "getCompiledApis" $ do
  compiledApis <- getCompiledApis
  let numApis = length compiledApis
  unless (numApis > 0) (assertFailure "no compiled apis")

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
  unless (deviceCount >= 0) (assertFailure "invalid device count")

main :: IO ()
main = defaultMain $ testGroup "RtAudio"
  [ testGetVersion
  , testGetCompiledApis
  , testApiName
  , testApiDisplayName
  , testApiByName
  , testAudio
  ]

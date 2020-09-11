module Main (main) where

import Control.Monad (unless)
import Sound.RtAudio
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

testGetVersion :: TestTree
testGetVersion = testCase "getVersion" $ do
  let expectedVersion = "5.1.0"
  actualVersion <- getVersion
  actualVersion @?= expectedVersion

testGetCompiledApis :: TestTree
testGetCompiledApis = testCase "getCompiledApis" $ do
  compiledApis <- getCompiledApis
  let numApis = length compiledApis
  unless (numApis > 0) (assertFailure "no compiled apis")

main :: IO ()
main = defaultMain $ testGroup "RtAudio"
  [ testGetVersion
  , testGetCompiledApis
  ]

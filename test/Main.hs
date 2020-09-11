module Main (main) where

import Sound.RtAudio (getVersion)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testGetVersion :: TestTree
testGetVersion = testCase "getVersion" $ do
  let expectedVersion = "5.1.0"
  actualVersion <- getVersion
  actualVersion @?= expectedVersion

main :: IO ()
main = defaultMain (testGroup "RtAudio" [testGetVersion])

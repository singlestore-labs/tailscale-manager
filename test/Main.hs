{-# LANGUAGE ImportQualifiedPost #-}

-- | Unit tests for TailscaleManager

module Main where

import Control.Monad (unless)
import Data.IP (IPRange)
import Data.Set qualified as S
import TailscaleManager as T
import TailscaleManager.Discovery.DNS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.AntXML (antXMLRunner)

-- | Asserts that the specified actual value is approximately equal to the
-- expected value. The output message will contain the prefix, the expected
-- value, the actual value, and the maximum margin of error.
--
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the expected and actual values are output.
--
-- (copied verbatim from HUnit-approx, but this version works with Tasty)
assertApproxEqual :: (HasCallStack, Ord a, Num a, Show a)
                  => String -- ^ The message prefix
                  -> a      -- ^ Maximum allowable margin of error
                  -> a      -- ^ The expected value
                  -> a      -- ^ The actual value
                  -> Assertion
assertApproxEqual preface epsilon expected actual =
  unless (abs (actual - expected) <= epsilon) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
              "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show epsilon ++ ")"

toIPRanges :: [String] -> S.Set IPRange
toIPRanges = S.fromList . map read

testShrinkage :: TestTree
testShrinkage = testGroup "shrinkRatio"
  [ testCase "0 shrink"        $ assertEqual "" 0.0  (T.shrinkRatio set1 set1)
  , testCase "25% shrinkage"   $ assertEqual "" 0.25 (T.shrinkRatio set1 set2)
  , testCase "100% shrinkage"  $ assertEqual "" 1.0  (T.shrinkRatio set1 S.empty)
  , testCase "negative shrink" $ assertApproxEqual "" epsilon (-0.333) (T.shrinkRatio set2 set1)
  ]
  where epsilon = 0.001
        set1 = toIPRanges [ "192.168.0.0/24"
                          , "192.168.1.0/24"
                          , "192.168.2.0/24"
                          , "192.168.3.0/24" ]
        set2 = toIPRanges [ "192.168.0.0/24"
                          , "192.168.1.0/24"
                          , "192.168.2.0/24" ]

-- | This may look redundant but I got it wrong the first time, so...
testIpToHostRoute :: TestTree
testIpToHostRoute = testGroup "ipToHostRoute"
  [ testCase "ipToHostRoute (ipv4)" $
      assertEqual "" (read "192.168.0.1/32") (ipToHostRoute $ read "192.168.0.1")
  , testCase "ipToHostRoute (ipv6)" $
      assertEqual "" (read "fd00::1/128") (ipToHostRoute $ read "fd00::1")
  ]

tests :: TestTree
tests = testGroup "TailscaleManager" [testShrinkage, testIpToHostRoute]

main :: IO ()
main = defaultMainWithIngredients ([antXMLRunner] <> defaultIngredients)
       tests

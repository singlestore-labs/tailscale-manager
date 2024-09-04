{-# LANGUAGE ImportQualifiedPost #-}

-- | Unit tests for TailscaleManager

module Main where

import Data.IP (IPRange)
import Data.Set qualified as S
import TailscaleManager as T
import Test.HUnit
import Test.HUnit.Approx (assertApproxEqual)

toIPRanges :: [String] -> S.Set IPRange
toIPRanges = S.fromList . map read

testShrinkage :: Test
testShrinkage = TestCase $ do
  let epsilon = 0.001
      set1 = toIPRanges [ "192.168.0.0/24"
                        , "192.168.1.0/24"
                        , "192.168.2.0/24"
                        , "192.168.3.0/24" ]
      set2 = toIPRanges [ "192.168.0.0/24"
                        , "192.168.1.0/24"
                        , "192.168.2.0/24" ]

  assertEqual "0 shrink"       0.0  (T.shrinkRatio set1 set1)
  assertEqual "25% shrinkage"  0.25 (T.shrinkRatio set1 set2)
  assertEqual "100% shrinkage" 1.0  (T.shrinkRatio set1 S.empty)
  assertApproxEqual "negative shrink" epsilon (-0.333) (T.shrinkRatio set2 set1)

-- | This may look redundant but I got it wrong the first time, so...
testIpToHostRoute :: Test
testIpToHostRoute = TestCase $ do
  assertEqual "ipv4 /32 route"
    (read "192.168.0.1/32")
    (ipToHostRoute $ read "192.168.0.1")
  assertEqual "ipv6 /128 route"
    (read "fd00::1/128")
    (ipToHostRoute $ read "fd00::1")

tests :: Test
tests =
  TestList
    [ TestLabel "shrinkRatio" testShrinkage,
      TestLabel "ipToHostRoute" testIpToHostRoute
    ]

main :: IO ()
main = runTestTTAndExit tests

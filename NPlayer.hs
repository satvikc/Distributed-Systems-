module NPlayer where

import Network
import Test.HUnit
import Test.QuickCheck





-- Tests

tests :: Test
tests = TestList $ map TestCase
  [assertEqual "add tests here"  1 (1::Int)
  ]

prop_empty :: Int -> Bool
prop_empty c1 = (c1::Int) == c1

runTests :: IO ()
runTests = do
  runTestTT tests
  quickCheck prop_empty

-- | For now, main will run our tests.
main :: IO ()
main = runTests
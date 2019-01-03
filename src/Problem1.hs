-- https://projecteuler.net/problem=1

-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

module Problem1 (
  solutionSlow,
  solutionFast,
  solutionTests
) where

import Test.HUnit

-- Brute force solution
multiplesOf :: [Int] -> [Int]
multiplesOf ds = filter (hasFactors ds) [1..]
  where
    hasFactors ds num = or $ map (\t -> num `rem` t == 0) ds

solutionSlow :: Int
solutionSlow = sum $ takeWhile (<1000) $ multiplesOf [3, 5]

-- Faster solution based on geometric sum
sumDivisibleBy :: Int -> Int -> Int
sumDivisibleBy n lim = (n * p * (p + 1)) `div` 2
  where
    p = lim `div` n

solutionFast :: Int
solutionFast = sumDivisibleBy 3 999 + sumDivisibleBy 5 999 - sumDivisibleBy 15 999

-- Tests
test1 = TestCase $ assertEqual "slow and fast solutions agree" solutionFast solutionSlow

solutionTests =
  runTestTT $ TestList [ test1 ]


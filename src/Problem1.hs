-- https://projecteuler.net/problem=1

-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

module Problem1 (
  solution_slow
) where

multiplesOf :: [Int] -> [Int]
multiplesOf ds = filter (hasFactors ds) [1..]
  where
    hasFactors ds num = or $ map (\t -> num `rem` t == 0) ds

solution_slow :: Int
solution_slow = sum $ takeWhile (<1000) $ multiplesOf [3, 5]

-----------------------------------------------------------------------------
-- Project Euler Solved Problems
-- Author: Mohamed Adam Chaieb
-----------------------------------------------------------------------------

import Math.Numbers
import Data.List

problem1 :: Integer
problem1 = sum $ filter (isMultipleOfOne [3,5]) [1..999]

problem2 :: Integer
problem2 = sum $ filter even $ fibonaccisUpTo 4000000

problem3 :: Integer
problem3 = last $ primeFactors 600851475143

problem4 :: Integer
problem4 = last  $ sort $ map undigits [digits (x*y)|x <- [100..999], y <- [100..999], isPalindrome $ digits $ x*y]
	where
		isPalindrome :: (Eq a) => [a] -> Bool
		isPalindrome x = (x == reverse x)

problem5 :: Integer
problem5 = error "Not implemented"

problem6 :: Integer
problem6 = ((^2) $ sum [1..100])-(sum [x^2 | x <-[1..100]])

problem7 :: Integer
problem7 = primes' !! 10001

problem8 = error "Not implemented"

problem9 :: Integer
problem9 = product $ head [[a,b,c] | a<-[200..1000], b<-[a..1000], c<-[b..1000], a^2+b^2 == c^2, a+b+c == 1000]

problem10 :: Integer
problem10 = sum $ primesUpTo 2000000
-----------------------------------------------------------------------------
-- Project Euler Solved Problems
-- Author: Mohamed Adam Chaieb
-----------------------------------------------------------------------------

import Data.List

-----------------------------------------------------------------------------
-----------------------  Helper Functions
-----------------------------------------------------------------------------

-- Divisibility predicate: checks if n is a multiple of x
isMultiple :: Integer -> Integer -> Bool
isMultiple x n = gcd x n == x

-- Divisibility predicate: checks if n is a multiple of any of multiple numbers
isMultipleOfOne :: [Integer] -> Integer -> Bool
isMultipleOfOne l n = foldl (\acc x -> if isMultiple x n then True else acc) False l

-- Divisibility predicate: checks if n is a multiple of any of multiple numbers
isMultipleOfAll :: [Integer] -> Integer -> Bool
isMultipleOfAll [] _ = False
isMultipleOfAll l n = foldl (\acc x -> if isMultiple x n then acc else False) True l

-- Prime generator: returns a list of all the prime numbers
primes' :: [Integer] 
primes' = listPrimes [2..]
	where
		listPrimes :: [Integer] -> [Integer] 
		listPrimes (x:l) = x:(listPrimes (removeMultiples x l))
		listPrimes [] = []

		isNotMultiple :: Integer -> Integer -> Bool
		isNotMultiple x n = not $ isMultiple x n

		removeMultiples = filter . isNotMultiple

-- Prime generator: returns a list of the n-th first prime numbers
primes :: Int -> [Integer]
primes n 
	| n >= 0 = (take n) primes' 
	| otherwise = []

-- Returns a list of all prime numbers less than a given Integer
primesUpTo :: Integer -> [Integer]
primesUpTo n = primesUpTo' n 0
	where
		primesUpTo' :: Integer -> Int -> [Integer]
		primesUpTo' n m
			| (primes' !! m) < n = (primes' !! m):(primesUpTo' n (m+1))
			| otherwise = []

-- Tests whether a number is prime (very inefficient)
isPrime :: Integer -> Bool
isPrime n = length (primeFactors n) == 1

-- Returns a list of all the sub-lists of the input list
subLists :: [a] -> [[a]]
subLists [] = [[]]
subLists (x:l) = (subLists l) ++ (map (x:) (subLists l)) 

-- Returns a list of the prime factors of a given Integer
primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = primeFactors' n primes'
	where
		primeFactors' :: Integer -> [Integer] -> [Integer]
		primeFactors' 1 _ = []
		primeFactors' n (x:l)
			| isMultiple x n = x:(primeFactors' (quot n x) (x:l))
			| otherwise = primeFactors' n l

-- Returns a list of tuples where the first element is the prime factor and the second is its exponent
primeDecompose :: Integer -> [(Integer, Int)]
primeDecompose n = let l = primeFactors n in zip l $ map length $ group l

-- Returns a list of divisors of a given number
divisors :: Integer -> [Integer]
divisors n = sort $ map product $ subLists $ primeFactors n

-- Returns the number of divisors of a given number
divisorsCount :: Integer -> Integer
divisorsCount n = 2^(length $ primeFactors n)

-- The factorial function
factorial :: Integer -> Integer
factorial n
	| n > 1 =  product [2..n] 
	| otherwise =  1

-- The Fibonacci sequence, using the analytic function form derived from the linear matrix equation form
fibonaccis :: [Integer]
fibonaccis = map fibonacci [0..]
	where
		fibonacci :: Integer -> Integer
		fibonacci n = round $ (1/sqrt 5)*((1+sqrt 5)/2)^n-(1/sqrt 5)*((1-sqrt 5)/2)^n

-- Returns a list of all Fibonacci numbers less than a given Integer
fibonaccisUpTo :: Integer -> [Integer]
fibonaccisUpTo n = fibonaccisUpTo' n 0
	where
		fibonaccisUpTo' :: Integer -> Int -> [Integer]
		fibonaccisUpTo' n m
			| (fibonacci m) < n = (fibonacci m):(fibonaccisUpTo' n (m+1))
			| otherwise = []

--Returns the n-th Fibonacci number
fibonacci :: Int -> Integer
fibonacci n = fibonaccis !! n

-- Returns a list of the digits
digits :: Integer -> [Integer]
digits n 
	| n < 10 = [n] 
	| otherwise = (digits $ quot n 10)++[(mod n 10)]

-- Contructs an Integer from the given list of digits
undigits :: [Integer] -> Integer
undigits [] = 0
undigits (x:l) = (x*10^(length l))+(undigits l)

-- Tests whether the given list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == reverse x)

-- Returns the n-th triangle number
triangle :: Integer -> Integer
triangle n = sum [1..n]

-- Checks if the number is perfect
isPerfect :: Integer -> Bool
isPerfect n = n == (sum $ init $ divisors n)

-----------------------------------------------------------------------------
----------------------- Problem Solutions
-----------------------------------------------------------------------------

problem1 :: Integer
problem1 = sum $ filter (isMultipleOfOne [3,5]) [1..999]

problem2 :: Integer
problem2 = sum $ filter even $ fibonaccisUpTo 4000000

problem3 :: Integer
problem3 = last $ primeFactors 600851475143

problem4 :: Integer
problem4 = last  $ sort $ map undigits [digits (x*y)|x <- [100..999], y <- [100..999], isPalindrome (digits (x*y))]

problem5 :: Integer
problem5 = error "Not implemented"

problem6 :: Integer
problem6 = (square $ sum [1..100])-(sum [square x | x <-[1..100]])
	where
		square :: Integer -> Integer
		square n = n*n

problem7 :: Integer
problem7 = primes' !! 10001

problem8 = error "Not implemented"

problem9 :: Integer
problem9 = product $ head [[a,b,c] | a<-[200..1000], b<-[a..1000], c<-[b..1000], a^2+b^2 == c^2, a+b+c == 1000]

problem10 :: Integer
problem10 = sum $ primesUpTo 2000000

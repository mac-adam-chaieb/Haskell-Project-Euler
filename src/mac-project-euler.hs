-------------------------------------------------------------------
-- Project Euler Solved Problems
-- Author: Mohamed Adam Chaieb
-------------------------------------------------------------------

import Data.List

-------------------------------------------------------------------
-----------------------  Helper Functions
-------------------------------------------------------------------

-- Divisibility predicate: checks if n is a multiple of x
isMultiple :: Integer -> Integer -> Bool
isMultiple x n = (gcd x n == x)

-- Divisibility predicate: checks if n is a multiple of any of multiple numbers
isMultipleOfOne :: [Integer] -> Integer -> Bool
isMultipleOfOne [] n = False
isMultipleOfOne (x:l) n = (isMultiple x n) || (isMultipleOfOne l n)

-- Divisibility predicate: checks if n is a multiple of any of multiple numbers
isMultipleOfAll :: [Integer] -> Integer -> Bool
isMultipleOfAll [] n = False
isMultipleOfAll (x:l) n = (isMultiple x n) && (isMultipleOfAll l n)

-- Prime generator: returns a list of all the prime numbers
primes' :: [Integer] 
primes' = listPrimes [2..]
	where
		listPrimes :: [Integer] -> [Integer] 
		listPrimes (x:l) = x:(listPrimes (removeMultiples x l))
		listPrimes [] = []

		isNotMultiple :: Integer -> Integer -> Bool
		isNotMultiple x n = not(isMultiple x n)

		removeMultiples = filter . isNotMultiple

-- Prime generator: returns a list of the n-th first prime numbers
primes :: Int -> [Integer]
primes n = if (n>(-1)) then (take n) primes' else []

-- Returns a list of all prime numbers less than a given integer
primesUpTo :: Integer -> [Integer]
primesUpTo n = primesUpTo' n 0
	where
		primesUpTo' :: Integer -> Int -> [Integer]
		primesUpTo' n m = 
			if (primes' !! m)<n then (primes' !! m):(primesUpTo' n (m+1))
			else []

-- Tests whether a number is prime (very inefficient)
isPrime :: Integer -> Bool
isPrime n = elem n primes'

-- Returns a list of the prime factors of a given integer
primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = primeFactors' n primes'
	where
		primeFactors' :: Integer -> [Integer] -> [Integer]
		primeFactors' 1 _ = []
		primeFactors' n (x:l) = 
			if(isMultiple x n)
			then x:(primeFactors' (quot n x) (x:l))
			else primeFactors' n l

-- The factorial function
factorial :: Integer -> Integer
factorial n = if(n>1) then product [2..n] else 1

-- The Fibonacci sequence, using the analytic function form derived from the linear matrix equation form
fibonaccis :: [Integer]
fibonaccis = (map fibonacci [3..])
	where
		fibonacci :: Integer -> Integer
		fibonacci n = round ((1/sqrt 5)*((1+sqrt 5)/2)^n-(1/sqrt 5)*((1-sqrt 5)/2)^n)

-- Returns a list of all Fibonacci numbers less than a given integer
fibonaccisUpTo :: Integer -> [Integer]
fibonaccisUpTo n = fibonaccisUpTo' n 0
	where
		fibonaccisUpTo' :: Integer -> Int -> [Integer]
		fibonaccisUpTo' n m = 
			if (fibonacci m)<n then (fibonacci m):(fibonaccisUpTo' n (m+1))
			else []

--Returns the n-th Fibonacci number
fibonacci :: Int -> Integer
fibonacci n = fibonaccis !! n

-- Returns a list of the digits
digits :: Integer -> [Integer]
digits n  = if (n<10) then [n] else (digits (quot n 10))++[(mod n 10)]

-- Contructs an integer from the given list of digits
undigits :: [Integer] -> Integer
undigits [] = 0
undigits (x:l) = (x*10^(length l))+(undigits l)

-- Tests whether the given list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == reverse x)

-------------------------------------------------------------------
----------------------- Problem Solutions
-------------------------------------------------------------------

problem1 :: Integer
problem1 = sum(filter (isMultipleOfOne [3,5]) [1..999])

problem2 :: Integer
problem2 = sum(filter even (fibonaccisUpTo 4000000))

problem3 :: Integer
problem3 = last (primeFactors 600851475143)

problem4 :: Integer
problem4 = last (sort(map undigits [digits (x*y)|x <- [100..999], y <- [100..999], isPalindrome (digits (x*y))]))

problem5 :: Integer
problem5 = error "Not implemented"

problem6 :: Integer
problem6 = (square (sum [1..100]))-(sum [square x | x <-[1..100]])
	where
		square :: Integer -> Integer
		square n = n*n

problem7 :: Integer
problem7 = primes' !! 10001
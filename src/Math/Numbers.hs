-----------------------------------------------------------------------------
-- Math - Numbers Module
-- Author: Mohamed Adam Chaieb
-----------------------------------------------------------------------------
module Math.Numbers
(
	digits,
	undigits,
	isMultiple,
	isMultipleOfOne,
	isMultipleOfAll,
	primes,
	primes',
	primesUpTo,
	primeFactors,
	isPrime,
	primeDecompose,
	divisors,
	divisorsCount,
	factorial,
	fibonacci,
	fibonaccisUpTo,
	triangle,
	isPerfect,
	pascal',
	pascal
)
where

import Data.List

-- Divisibility predicate: checks if n is a multiple of x
isMultiple :: Integer -> Integer -> Bool
isMultiple x n = gcd x n == x

-- Divisibility predicate: checks if n is a multiple of any of multiple numbers
isMultipleOfOne :: [Integer] -> Integer -> Bool
isMultipleOfOne l n = foldl' (\acc x -> if isMultiple x n then True else acc) False l

-- Divisibility predicate: checks if n is a multiple of any of multiple numbers
isMultipleOfAll :: [Integer] -> Integer -> Bool
isMultipleOfAll [] _ = False
isMultipleOfAll l n = foldl' (\acc x -> if isMultiple x n then acc else False) True l

-- Prime generator: returns a list of all the prime numbers
primes' :: [Integer] 
primes' = listPrimes [2..]
	where
		listPrimes :: [Integer] -> [Integer] 
		listPrimes l = foldr (\x acc -> x:(removeMultiples x acc)) [] l

		removeMultiples x l = filter (\n -> not $ isMultiple x n) l

-- Prime generator: returns a list of the n-th first prime numbers
primes :: Int -> [Integer]
primes n 
	| n >= 0 = take n $ primes' 
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
subLists l = foldl (\acc x -> (map (x:) acc) ++ acc) [[]] l

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
primeDecompose :: Integer -> [(Integer, Integer)]
primeDecompose n = let l = primeFactors n in zip l $ map toInteger $ map length $ group l

-- Returns a list of divisors of a given number
divisors :: Integer -> [Integer]
divisors n = nub $ sort $ map product $ subsequences $ primeFactors n

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

-- Returns the n-th triangle number
triangle :: Integer -> Integer
triangle n = sum [1..n]

-- Checks if the number is perfect
isPerfect :: Integer -> Bool
isPerfect n = n == (sum $ init $ divisors n)

-- Returns Pascal's triangle
pascal' :: [[Integer]]
pascal' = zipWith (\a b -> map combinations $ zip b a) (map (\x -> [0.. toInteger x]) [0..]) $ map (\y -> replicate (y+1) (toInteger y)) [0..]
	where
		combinations :: (Integer, Integer) -> Integer
		combinations (n,m) = quot (factorial n) ((factorial m)*(factorial $ n-m))

-- Return the (n,m) entry of Pascal's triangle
pascal :: Int -> Int -> Integer
pascal n m = (pascal' !! n) !! m
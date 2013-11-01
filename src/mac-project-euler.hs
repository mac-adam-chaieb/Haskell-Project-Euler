-------------------------------------------------------------------
-- Project Euler Solved Problems
-- Author: Mohamed Adam Chaieb
-------------------------------------------------------------------

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

-- Prime generator: returns a list of all the prime numbers
primes' :: [Integer] 
primes' =
	let
		listPrimes :: [Integer] -> [Integer] 
		listPrimes (x:l) = x:(listPrimes (removeMultiples x l))
		listPrimes [] = []

		isNotMultiple :: Integer -> Integer -> Bool
		isNotMultiple x n = not(isMultiple x n)

		removeMultiples = filter . isNotMultiple
	in listPrimes [2..]

-- Prime generator: returns a list of the n-th first prime numbers
primes :: Int -> [Integer]
primes n = if (n>(-1)) then (take n) primes' else []

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

-- Contructs an integer from the given list of digits
undigits :: [Int] -> Int
undigits [] = 0
undigits (x:l) = (x*10^(length l))+(undigits l)

-------------------------------------------------------------------
----------------------- Problem Solutions
-------------------------------------------------------------------

problem1 :: Integer
problem1 = sum(filter (isMultipleOfOne [3,5]) [1..999])

problem2 :: Integer
problem2 = sum(filter even (fibonaccisUpTo 4000000))
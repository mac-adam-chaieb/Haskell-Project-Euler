-- Project Euler Solved Problems
-- Author: Mohamed Adam Chaieb

-- Helper Functions
primes' :: [Integer] 
primes' = let 
			listPrimes :: [Integer] => [Integer] 
			listPrimes (x:l) = x:(listPrimes (removeMultiples x l))
			listPrimes [] = []

			isNotMultiple :: Integer -> Integer -> Bool
			isNotMultiple x n = (gcd x n /= x)

			removeMultiples = filter . isNotMultiple
		  in listPrimes [2..]

primes :: Int -> [Integer]
primes n = if (n>(-1)) then (take n) primes' else []
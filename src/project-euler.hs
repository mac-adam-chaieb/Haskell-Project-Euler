-----------------------------------------------------------------------------
-- Project Euler Solved Problems
-- Author: Mohamed Adam Chaieb
-----------------------------------------------------------------------------
-- Description: This program prompts the user to enter a problem number, and 
-- returns the answer. Solutions are then cached in a file in order to avoid 
-- computing solutions multiple times.
-----------------------------------------------------------------------------

import Math.Numbers
import Data.List
import qualified Data.Map as Map
import Control.Monad
import System.IO

-- Mapping of the problem numbers
problems = Map.fromList [(1, problem1),
						 (2, problem2),
						 (3, problem3),
						 (4, problem4),
						 (6, problem6),
						 (7, problem7),
						 (9, problem9),
						 (10, problem10),
						 (14, problem14),
						 (15, problem15)]

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

problem6 :: Integer
problem6 = ((^2) $ sum [1..100])-(sum [x^2 | x <-[1..100]])

problem7 :: Integer
problem7 = primes' !! 10001


problem9 :: Integer
problem9 = product $ head [[a,b,c] | a<-[200..1000], b<-[a..1000], c<-[b..1000], a^2+b^2 == c^2, a+b+c == 1000]

problem10 :: Integer
problem10 = sum $ primesUpTo 2000000

problem14 :: Integer
problem14 = fst $ getMaxPair (0,0) $ map (\n -> (n, length $ collatz n)) [1..1000000]
	where
		-- uses an accumulator
		getMaxPair :: Ord b => (a,b) ->[(a,b)] -> (a,b)
		getMaxPair p [] = p 
		getMaxPair p ((x,y):l) = if (y > snd p) then getMaxPair (x,y) l else getMaxPair p l

problem15 :: Integer
problem15 = pascal 40 20

main = do
	putStrLn "Welcome to the Project Euler Problem Solver!"
	forever $ do
		--First read the exisiting solutions, store them in solutionMap
		readValue <- readFile "solutions.txt"
		text <- return readValue
		let solutionMap = Map.fromList $ map (\x -> (case x of [a,b] -> (a,b))) $ map words $ lines text
		putStrLn "Please enter the number of the problem you're seeking the answer for:"
		n <- getLine
		if(Map.member n solutionMap) 
		then putStrLn $ "The answer to problem " ++ n ++ " is " ++ (show $ solutionMap Map.! n)
		else if(Map.member (read n) problems)
			then do
				putStrLn $ "Computing the answer to problem " ++ n ++ "..."
				putStrLn $ "The answer to problem " ++ n ++ " is " ++ (show $ problems Map.! (read n))
				-- write the solution to the solutions file
				appendFile "solutions.txt" $ n ++ "\t" ++ (show $ problems Map.! (read n)) ++ "\n"
			else putStrLn "This problem doesn't have a solution yet!"
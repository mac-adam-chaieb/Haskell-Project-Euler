-----------------------------------------------------------------------------
-- Math - Graphs Module
-- Author: Mohamed Adam Chaieb
-----------------------------------------------------------------------------

module Math.Graphs
(
	Tree,
	Math.Graphs.map,
	insert,
	merge
)
where

import qualified Data.Set as S

-- Type definition of a connected tree
data Tree a = Empty | Node a [Tree a] deriving (Show)

-- Map function for trees
map :: (a -> b) -> Tree a -> Tree b
map f Empty = Empty
map f (Node a l) = Node (f a) (Prelude.map (Math.Graphs.map f) l)

-- Insertion: inserts an element randomly in the tree
insert :: a -> Tree a -> Tree a
insert x Empty = Node x []
insert x (Node n []) = Node n [Node x []]
insert x (Node n l) = Node n ((insert x $ random l):(tail l))
	where
		-- will implement this later
		random :: [a] -> a
		random l = head l

-- Removal: removes an element from a tree
remove :: (Eq a) => a -> Tree a -> Tree a
remove x Empty = Empty
-- remove x (Node a l) = if a == x then 

-- Merge trees under one root
merge :: [Tree a] -> Tree a
merge [] = Empty
merge (Empty:l) = merge l
merge ((Node a c):l) = Node a (c++l)  
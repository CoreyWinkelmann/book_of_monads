module Lib where

data Tree a = Leaf a | Node (Tree a) (Tree a)

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = (numberOfLeaves l) + (numberOfLeaves r)

relabel :: Tree a -> Integer -> (Tree (Integer, a), Integer)
relabel (Leaf x) i = (Leaf (i, x), i + 1)
relabel (Node l r) i = let (l', i1) = relabel l i
                           (r', i2) = relabel r i1
                        in (Node l' r', i2)

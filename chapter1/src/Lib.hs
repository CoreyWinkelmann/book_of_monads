{-# LANGUAGE TupleSections #-}

module Lib where

import Prelude hiding (pure)

-- Section 1.1

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = (numberOfLeaves l) + (numberOfLeaves r)

relabel :: Tree a -> Integer -> (Tree (Integer, a), Integer)
relabel (Leaf x) i = (Leaf (i, x), i + 1)
relabel (Node l r) i = let (l', i1) = relabel l i
                           (r', i2) = relabel r i1
                        in (Node l' r', i2)

-- I am doing a stack ghci of this repo
-- let t = Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4)
-- relabel t
-- numberOfLeaves t

type WithCounter a = Integer -> (a, Integer)

-- I believe this has a similar shape to monad's bind function
next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g = \i -> let (r, i') = f i in g r i'

-- Added the TupleSections language extension to simplify this definition
pure :: a -> WithCounter a
pure x = (x,)

-- Redo relabel with the new WithCounter type
relabel' :: Tree a -> WithCounter (Tree (Integer, a))
relabel' (Leaf x) = \i -> (Leaf (i, x), i + 1)
relabel' (Node l r) = relabel' l `next` \l' ->
                      relabel' r `next` \r' ->
                      pure (Node l' r')

type State s a = s -> (a, s)

-- Exercise 1.1
next' :: State Integer a -> (a -> State Integer b) -> State Integer b
f `next'` g = \i -> let (r, i') = f i in g r i'

pure' :: a -> State Integer a
pure' x = (x,)

-- Section 1.2

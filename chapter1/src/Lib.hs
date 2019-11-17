{-# LANGUAGE TupleSections #-}

module Lib where

import Prelude hiding (pure, (++), Maybe, Just, Nothing)

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
next' :: State s a -> (a -> State s b) -> State s b
f `next'` g = \i -> let (r, i') = f i in g r i'

pure' :: a -> State Integer a
pure' x = (x,)

-- Section 1.2

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Exercise 1.2
(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

-- Exercise 1.3
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

singleton :: a -> [a]
singleton x = [x]

concat :: [[a]] -> [a]
concat = foldr (++) []

-- Section 1.3

data Maybe a = Nothing | Just a deriving (Show)

type Name = String
data Person = Person { name :: Name, age :: Integer } deriving (Show)

validateName :: String -> Maybe Name
validateName [] = Nothing
validateName name = Just name

validateAge :: Integer -> Maybe Integer
validateAge a | a > 0 = Just a
              | otherwise = Nothing

validatePerson :: String -> Integer -> Maybe Person
validatePerson name age = 
  validateName name `then_` \name' ->
    validateAge age `then_` \age' ->
      Just $ Person name' age'

-- validatePerson :: String -> Integer -> Maybe Person
-- validatePerson name age = 
--   case validateName name of
--     Nothing -> Nothing
--     Just name' -> case validateAge age of
--       Nothing -> Nothing
--       Just age' -> Just $ Person name' age'

then_ :: Maybe a -> (a -> Maybe b) -> Maybe b
then_ v g = case v of
              Nothing -> Nothing
              Just v' -> g v'

map'' :: (a -> b) -> Maybe a -> Maybe b
map'' f Nothing = Nothing
map'' f (Just x) = Just $ f x

singleton' :: a -> Maybe a
singleton' = Just

flatten' :: Maybe (Maybe a) -> Maybe a
flatten' (Just (Just x)) = Just x
flatten' _ = Nothing

flatten'' :: Maybe (Maybe a) -> Maybe a
flatten'' oo = then_ oo id

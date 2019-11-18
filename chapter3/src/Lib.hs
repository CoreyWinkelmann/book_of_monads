module Lib where

-- One description: applying a function in whatever is inside of a container f
-- Another: If we have a pure function g, we write fmap g for the version that consumes and produces values in the context denoted by f.
--          We often say g has been lifted into that functor.
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

plus :: Maybe Int -> Maybe Int -> Maybe Int
plus x y = do a <- x
              b <- y
              return (a + b)

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f x y = do a <- x
                 b <- y
                 return (f a b)

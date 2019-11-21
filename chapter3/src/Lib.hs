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

-- Ex. 3.2
fmap' :: Applicative f => (a -> b) -> f a -> f b
fmap' f b = pure f <*> b

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

-- Ex. 3.3
instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap f zxs = ZipList $ map f $ getZipList zxs

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)



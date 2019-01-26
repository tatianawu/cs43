module Main where

import Data.Semigroup (stimes) -- stimes not imported by default


main :: IO ()
main = do
  putStrLn "hello world"


newtype Product = Product Int deriving (Show)
newtype Sum = Sum Int deriving (Show)

instance Semigroup Sum where
    (Sum a) <> (Sum b) = Sum $ a + b -- define <>
    
    stimes n (Sum x)
      | n <= 0    = error "positive multiplier expected"
      | otherwise = Sum $ x * (fromIntegral n)
    
instance Semigroup Product where
    (Product a) <> (Product b) = Product $ a * b
    
    stimes n (Product x)
      | n <= 0    = error "positive multiplier expected"
      | otherwise = Product $ x ^ (fromIntegral n)


stimes' :: (Semigroup a, Integral b) => b -> a -> a
stimes' n x
  | n <= 0    = error "positive multiplier expected"
  | n == 1    = x
  | even n    = stimes' m x <> stimes' m x
  | otherwise = stimes' (m + 1) x <> stimes' m x
  where m = n `div` 2


{-# LANGUAGE InstanceSigs #-}

module Main where

import Prelude hiding (Left, Right)

main :: IO ()
main = do
  putStrLn "Assignment 3"
  putStrLn "Tatiana Wu"

-- Problem 1
-- Define an instance of Functor for Either e for a fixed e.

data Either' a b = Left a | Right b

instance Functor (Either' e) where
    fmap :: (a -> b) -> Either' e a -> Either' e b
    fmap f (Left a) = Left a
    fmap f (Right b) = Right $ f b

-- Problem 2
-- Define an instance of Functor for a rose tree.

data Tree a = Node a [Tree a]

instance Functor (Tree) where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node a as) = Node (f a) $ fmap (fmap f) as

-- Problem 3
-- True/False: The composition of two Functors is also a Functor

{- True
 -
 - Let f be a Functor f :: (a -> b) -> f a -> f b
 - Let g be a Functor g :: (a -> b) -> g a -> g b
 -
 - If we compose f and g, we see that:
 -      (f . g) :: (a -> b) -> f (g a) -> f (g b)
 -}


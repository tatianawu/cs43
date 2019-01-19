module Main where

main :: IO ()
main = do
  putStrLn "Assignment 1"
  putStrLn "Tatiana Wu"

-- Problem 1
-- Implement the map function using a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x) : xs) []

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldl (\xs x -> xs ++ [(f x)]) []


-- Problem 2
-- Implement the filter function using a fold
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if (f x) then x : xs else xs) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldl (\xs x -> xs ++ if (f x) then [x] else []) []


-- Problem 3
-- Implement foldl using foldr

-- Bad approach: two foldr's
reverse' = foldr (\x y -> y ++ [x]) [] -- reverse helper using foldr
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc xs = foldr (\x y -> f y x) acc $ reverse' xs

-- Better approah
-- foldr returns a function g :: (b -> b) -> b -> b
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f acc xs = (foldr (\a g b -> g (f b a)) id xs) acc


-- Problem 4
-- Compute the smallest positive number that is evenly divisible by [1..n]
-- Provide an answer for n = 20


-- Problem 5
-- Compute the nth prime number
-- Provide an answe3r for n = 10001

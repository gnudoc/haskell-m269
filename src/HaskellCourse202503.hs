module Haskellcourse202503 where

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

third :: [a] -> a
third a = head $ tail $ tail a

third' :: [a] -> a
third' a = a !! 2

third'' :: [a] -> a
third'' (x1:x2:x3:xs) = x3

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs   = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

myAND01 :: Bool -> Bool -> Bool
myAND01 True True = True
myAND01 True False = False
myAND01 False True = False
myAND01 False False = False

myAND02 :: Bool -> Bool -> Bool
myAND02 True True = True
myAND02 _ _ = False

myAND03 :: Bool -> Bool -> Bool
myAND03 True a = a
myAND03 False _ = False

myAND04 :: Bool -> Bool -> Bool
myAND04 False False = False
myAND04 a b = a == b

myOR01 :: Bool -> Bool -> Bool
myOR01 True True = True
myOR01 True False = True
myOR01 False True = True
myOR01 False False = False

myOR02 :: Bool -> Bool -> Bool
myOR02 False False = False
myOR02 _ _ = True

myOR03 :: Bool -> Bool -> Bool
myOR03 False a = a
myOR03 True _ = True

myOR04 :: Bool -> Bool -> Bool
myOR04 True True = True
myOR04 a b = a /= b

myAnd05 :: Bool -> Bool -> Bool
myAnd05 a b =
  if a then
    if b then True else False
  else False

myAnd06 :: Bool -> Bool -> Bool
myAnd06 a b =
  if a then b
  else False

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble :: Int -> Int
luhnDouble x
  | 2*x < 10   = 2*x
  | otherwise  = 2*x - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

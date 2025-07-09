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

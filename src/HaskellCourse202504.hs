module Haskellcourse202504 where
allPairs01 :: [(Integer,Integer)]
allPairs01 = [(x,y) | x <- [0..], y <- [0..]]

allPairs02 :: [(Integer,Integer)]
allPairs02 = [(x,diag-x) | diag <- [0..], x <- [0..diag]]
disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint _ [] = True
disjoint [] _ = True
disjoint (x:xs) (y:ys)
  | x < y     = disjoint xs (y:ys)
  | x == y    = False
  | x > y     = disjoint (x:xs) ys
ram5s n = [(a,b,c,d,a^3 + b^3)
          | a <- [1..n], b <- [a..n], c <- [a+1..n], d <- [c..n], a^3 + b^3 == c^3 + d^3
          ]
map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = [f x | x <- xs]
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x:xs) =
  if p x then x : filter1 p xs
         else filter p xs
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p xs =[x | x <- xs, p x]
sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

product1 :: Num a => [a] -> a
product1 [] = 1
product1 (x:xs) = x * product1 xs
foldr01 f v [] = v -- v is the "final value"
foldr01 f v (x:xs) = f x (foldr01 f v xs)
concat1 :: [[a]] -> [a]
concat1 xss = foldr01 (++) [] xss
map3 :: (a -> b) -> [a] -> [b]
map3 f xs = foldr01 ((:) . f) [] xs -- so the function we apply in each fold is a combination of function to be mapped and the `:`

-- or as a lambda:
map4 :: (a -> b) -> [a] -> [b]
map4 f xs = foldr01 (\x xs -> f x : xs) [] xs

import Prelude hiding ((!!))

-- 05- Chose the correct definition for the function that concatenates
-- a list of lists

c2 :: [[a]] -> [a]
c2 [] = []
c2 (xs:xss) = xs ++ c2 xss

-- 06- Choose the correct definition for the function that produces a
-- list with n identical elements:

rep :: Int -> a -> [a]
rep 0 _ = []
rep n x = x : rep (n-1) x

-- 07- Choose the correct definition for the function that selects the
-- n th element of a list. We start counting at 0.

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)

-- 08- Choose the correct definition for the function that decides if
-- a value is an element of a list:

el :: Eq a => a -> [a] -> Bool
el _ [] = False
el x (y:ys)
  | x == y = True
  | otherwise = el x ys

-- 09- Choose the correct definition for the function merge :: Ord a
-- => [a] -> [a] -> [a] that merges two sorted lists in ascending
-- order to give a single sorted list in ascending order. For example:

-- merge [2, 5, 6] [1, 3, 4]
-- [1, 2, 3, 4, 5, 6]

m2 [] ys = ys
m2 xs [] = xs
m2 (x:xs) (y:ys) = if x <= y then x : m2 xs (y:ys) else y : m2 (x:xs) ys


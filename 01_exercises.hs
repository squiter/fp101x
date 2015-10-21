-- Introduction
-- 01
n = a `div` length xs
    where a  = 10
          xs = [1,2,3,4,5]

-- 02
-- 03
-- 04
-- 05
-- 06
-- 07
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                 smaller = [a | a <- xs, a <= x]
                 larger = [b | b <- xs, b > x]

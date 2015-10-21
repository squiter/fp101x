-- 1: Give another possibility calculation for the result of `double
-- (double 2)`

-- double ( double 2 )
-- double ( 2 + 2 )
-- 2 + 2 + 2 + 2
-- 8

-- (double 2) + (double 2)
-- 2 + 2 + 2 + 2
-- 8

-- ######################################################################

-- 2: Show that `sum [x] = x` for any `x`
-- I don't understand the second question -.-'

-- sum [2,3,4]
-- 2 + sum [3,4]
-- 2 + ( 3 + sum [4])
-- 2 + ( 3 + ( 4 + sum []))
-- 2 + ( 3 + ( 4 + (0)))
-- 9

-- ######################################################################

-- 3: Define a function `product` that produce the product of a list
-- of numberand show using your definition that `product [2,3,4] =
-- 24`.

product' :: Num x => [x] -> x
product' [] = 1
product' (x:xs) = x * product' xs

-- product' [2,3,4]
-- 2 * product' [3,4]
-- 2 * ( 3 * prodcut' [4] )
-- 2 * ( 3 * ( 4 * product [] ))
-- 2 * (3 * (4 * ([])))
-- 24

-- ######################################################################

-- 4: How should the definition of the function `qsort` be modified so
-- that it produce a `reverse` sorted version of a list?

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]

rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]

-- ######################################################################

-- 5: What should be the effect of replacing `<=` by `<` in the
-- definition of `qsort`? Hint: consider the example: `qsort
-- [2,2,3,1,1]`

-- The q sort function will never get the duplicated numbers;
-- This changes will return: [1,2,3]

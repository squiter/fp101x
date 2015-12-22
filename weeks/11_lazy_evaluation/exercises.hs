-- 06- Choose the correct implementations for the expression fibs ::
-- [Integer] that generates the infinite sequence of Fibonacci numbers
-- (i.e. [0, 1, 1, 2, ...]).

-- f1 :: [Integer]
-- f1 = 1 : [x + y | (x,y) <- zip f1 (tail f1)]

-- f2 :: [Integer]
-- f2 = 0 : 1 : zipWith (*) f2 (tail f2)

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs  (tail fibs)]

-- f4 :: [Integer]
-- f4 = 1 : 1 : [x + y | (x,y) <- zip (tail f4) f4]

-- 07- Using fibs from the previous exercise, choose the correct
-- definition for the function fib :: Int -> Integer that returns the
-- n-th Fibonnaci number (counting from zero).

fib :: Int -> Integer
fib n = fibs !! n

-- 08- Choose the correct definition for the expression largeFib ::
-- Integer that uses fibs from the previous exercises to calculate the
-- first Fibonacci number greater than 1000.

largeFib :: Integer
largeFib = head (dropWhile (<=1000) fibs)

-- 09- Choose the correct definition for the function repeatTree :: a
-- -> Tree a for the following type of binary trees:

data Tree a = Leaf
            | Node (Tree a) a (Tree a)

-- The behavior should be analogous to that of the library function
-- repeat (not considering bottom and partial cases):

repeat :: a -> [a]
repeat x = xs
  where xs = x : xs

-- rt1 :: a -> Tree a
-- rt1 x = Node x x x

rt2 :: a -> Tree a
rt2 x = Node t x t
  where t = rt2 x

-- rt3 :: a -> Tree a
-- rt3 x = rt3 (Node Leaf x Leaf)

-- rt4 :: a -> Tree a
-- rt4 x = Node t x t
--   where t = rt4 (Node t x t)

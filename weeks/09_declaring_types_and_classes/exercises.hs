--  Select all possible total and terminating implementations of a
--  conversion function natToInteger :: Nat -> Integer that converts
--  any non-bottom, non-partial, finite natural number (note: 0 is a
--  natural number according to this definition), into the
--  corresponding Integer value.

import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)

data Nat = Zero
         | Succ Nat
         deriving Show

nti1 Zero = 0
nti1 (Succ n) = nti1 n + 1

nti2 (Succ n) = nti2 n + 1
nti2 Zero = 0

nti4 (Succ n) = 1 +  nti4 n
nti4 Zero = 0

nti6 = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

nti7 :: Nat -> Integer
nti7 = \n -> genericLength [c | c <- show n, c == 'S']


-- 01- Select all possible total and terminating implementations of a
-- conversion function integerToNat :: Integer -> Nat that converts
-- any non-bottom, non-partial, finite Integer value >= 0, into the
-- corresponding Nat value.

itn1 0 = Zero
itn1 (n+1) = Succ (itn1 n)

itn5 (n+1) = Succ (itn5 n)
itn5 0 = Zero

itn6 (n+1) = let m = itn6 n in Succ m
itn6 0 = Zero

-- 02 - Select all possible total and terminating implementations of
-- an addition function add :: Nat -> Nat -> Nat that adds two
-- non-bottom, non-partial, finite natural numbers m and n, such that
-- natToInteger (add m n) = natToInteger m + natToInteger n.

add Zero n = n
add (Succ m) n = Succ (add n m)

a2 (Succ m) n = Succ (a2 n m)
a2 Zero n = n

a7 n Zero = n
a7 n (Succ m) = Succ (a7 m n)

a8 n (Succ m) = Succ (a8 m n)
a8 n Zero = n

-- 03 - Using recursion, and any correct implementation of the
-- function add from the previous exercise, select from the following
-- options, a total and terminating multiplication function mult ::
-- Nat -> Nat -> Nat that multiplies two non-bottom, non-partial,
-- finite natural numbers m and n, such that natToInteger (mult m n) =
-- natToInteger m * natToInteger n.

mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

-- 04 - Given the following data type for trees with Integers at the
-- leafs and inside the nodes:

-- data Tree = Leaf Integer
--           | Node Tree Integer Tree

-- Select all correct implementations of the function

-- occurs :: Integer -> Tree -> Bool

--  that decides whether the given Integer occurs in the given
--  Tree. The Tree parameter is a finite, non-partial, non-bottom
--  binary search tree.

-- Note: If you haven't encountered case expressions before, Google is your friend.

-- tree = Node (Node (Leaf 1) 3 (Leaf 5)) 6 (Node (Leaf 7) 8 (Leaf 9))

-- o1 :: Integer -> Tree -> Bool
-- o1 m (Leaf n) = m == n
-- o1 m (Node l n r)
--   = case compare m n of
--         LT -> o1 m l
--         EQ -> True
--         GT -> o1 m r

-- o5 :: Integer -> Tree -> Bool
-- o5 m (Leaf n) = m == n
-- o5 m (Node l n r)
--   | m == n = True
--   | m < n = o5 m l
--   | otherwise = o5 m r

-- 05- Consider Tree.

data Tree = Leaf Integer
          | Node Tree Tree

-- We say that a tree is balanced if the number of
-- leaves in the left and right subtree of every node differs by at
-- most one, with leaves themselves being trivially balanced.

-- Which option correctly implements

-- balanced :: Tree -> Bool

--  that decides if a finite, non-partial, non-bottom binary tree is
--  balanced or not?

bt1 = Node (Node (Leaf 1) (Leaf 5)) (Node (Leaf 7) (Leaf 9))
bt2 = Node (Node (Leaf 1) (Leaf 5)) (Node (Leaf 7) (Leaf 10))
ut1 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Node (Leaf 5) (Leaf 6))))

balanced :: Tree -> Bool
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
balanced (Leaf _) = True
balanced (Node l r)
  = abs (leaves l - leaves r) <=1 && balanced l && balanced r

-- 06- Given the definition of binary trees from the previous
-- exercise, define a function

balance :: [Integer] -> Tree

--  that converts a finite, non-empty, non-partial, non-bottom list of
--  non-bottom integers into a balanced tree.

halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
  where (ys, zs) = halve xs

-- 07- The expression Add (Val 1) (Val 2) is a value of the datatype:

-- data Exp = Add Exp Exp | Val Int

-- 08 - The expression Node (Leaf 1) (Leaf 2) is a value of the
-- datatype:

-- data Tree = Leaf Int | Node Tree Tree

-- 09- Given the algebraic data type data Maybe a = Nothing | Just a,
-- pick the correct instance declaration that shows that the type
-- constructor Maybe is a Monad. Assume that all values of type Maybe
-- a are finite, non-partial, and non-bottom. You don't have to prove
-- that the Monad laws hold, but use your common sense when picking
-- the right answer.

-- data Maybe a = Nothing | Just a
-- instance Monad Maybe  where
--   return x = Just x
--   Nothing >>= _ = Nothing
--   (Just x) >>= f = f x

-- 10- Given the list type from the standard Prelude, pick the correct
-- instance declaration that shows that the type constructor [] is a
-- Monad. Assume that all values of type [a] are finite, non-partial,
-- and non-bottom. You don't have to prove that the Monad laws hold,
-- but use your common sense when picking the right answer.

instance Monad [] where
  return x = [x]
  xs >>= f = concat(map f xs)

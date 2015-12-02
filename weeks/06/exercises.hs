-- 00- Choose the equivalent of the following list comprehension [f x
-- | x <- xs, p x] expressed using higher-order functions.

-- map f (filter p xs)

-- ###########################################################################
-- 01- Choose all options that implement the Prelude function
-- all :: (a -> Bool) -> [a] -> Bool

-- taking into account only finite, non-partial input lists with
-- non-bottom values and where the predicate p always returns either
-- True, or False, but not bottom.

all1 p xs = and (map p xs)
-- :failed: all2 p xs = map p (and xs)
all3 p = and . map p
all4 p = not . any (not . p)
-- :failed: all5 p = map p . and
all6 p xs = foldl (&&) True (map p xs)
-- :failed: all7 p xs = foldr (&&) False (map p xs)
all8 p = foldr (&&) True . map p

-- For additional understanding, try to experiment with infinite and
-- partial lists and see if you can spot any differences in behaviour
-- for the various implementations.

-- ###########################################################################
-- 02- Choose all options that implement the Prelude function any any
-- :: (a -> Bool) -> [a] -> Bool taking into account only finite,
-- non-partial input lists with non-bottom values and where the
-- predicate p always returns either True, or False, but not bottom.

-- :failed: any1 p = map p . or
any2 p = or . map p
any3 p xs = length (filter p xs) > 0
-- :failed: any4 p = not . null dropWhile (not . p)
-- :failed: any5 p = null . filter p
any6 p xs = not (all (\ x -> not (p x)) xs)
any7 p xs = foldr (\ x acc -> (p x) || acc) False xs
-- :failed: any8 p xs = foldr (||) True (map p xs)

-- ###########################################################################
-- 03- Choose the option that implements the Prelude function:
-- takeWhile :: (a -> Bool) -> [a] -> [a] taking into account only
-- finite, non-partial input lists with non-bottom values and where
-- the predicate p always returns either True, or False, but not
-- bottom.

-- This function is like a filter
-- tw1 _ [] = []
-- tw1 p (x:xs)
--   | p x = x : tw1 p xs
--   | otherwise = tw1 p xs

tw2 _ [] = []
tw2 p (x:xs)
  | p x = x : tw2 p xs
  | otherwise = []

-- Don't compile
-- tw3 _ [] = []
-- tw3 p (x:xs)
--   | p x = tw3 xs
--   | otherwise = []

-- invert the array
-- tw4 p = foldl (\ acc x -> if p x then x : acc else acc) []

-- ###########################################################################
-- 04- Choose the option that implements the Prelude function
-- dropWhile (a -> Bool) -> [a] -> [a] taking into account only
-- finite, non-partial input lists with non-bottom values and where
-- the predicate p always returns either True, or False, but not
-- bottom.

dw1 _ [] = []
dw1 p (x:xs)
  | p x = dw1 p xs
  | otherwise = x : xs

-- dw2 _ [] = []
-- dw2 p (x:xs)
--   | p x = dw2 p xs
--   | otherwise = xs

-- dw3 p = foldr (\ x acc -> if p x then acc else x : acc) []

-- dw4 p = foldl add []
--   where add [] x = if p x then [] else [x]
--         add acc x = x : acc

-- ###########################################################################
-- 05- Choose the option that implements the Prelude function
-- map (a -> b) -> [a] -> [b] taking into account only finite, non-partial
-- input lists with non-bottom values and where the mapping function
-- does not return bottom.

-- m1 f = foldr (\ x xs -> xs ++ [f x]) [] -- Invert results
-- m2 f = foldr (\ x xs -> f x ++ xs) [] -- raise an error
-- m3 f = foldl (\ xs x -> f x : xs) [] -- Same result for m1
m4 f = foldl( \ xs x -> xs ++ [f x]) []

-- ###########################################################################
-- 06- Choose the option that implements the Prelude function
-- filter :: (a -> Bool) -> [a] -> [a] taking into account only finite,
-- non-partial input lists with non-bottom values and where the
-- predicate p always returns either True, or False, but not bottom.

-- f1 p = foldl (\ xs x -> if p x then x : xs else xs) [] -- Inverts
f2 p = foldr (\ x xs -> if p x then x : xs else xs) []
-- f3 p = foldr (\ x xs -> if p x then xs ++ [x] else xs) [] -- Inverts
-- f4 p = foldl (\ x xs -> if p x then xs ++ [x] else xs) [] -- Don't compile

-- ###########################################################################
-- 07- Choose a definition for the function dec2int :: [Integer] ->
-- Integer that converts a finite, non-partial list of non-bottom
-- Integer digits, that represents a decimal number, into the
-- non-bottom Integer this list represents. For example:
-- > dec2int [2, 3, 4, 5]
-- 2345
-- > dec2int []
-- 0
-- > dec2int [0, 0, 0, 0]
-- 0

-- d2i1 :: [Integer] -> Integer
-- d2i1 = foldr (\ x y -> 10 * x + y) 0
-- d2i2 :: [Integer] -> Integer
-- d2i2 = foldl (\ x y -> x + 10 * y) 0
d2i3 :: [Integer] -> Integer
d2i3 = foldl (\ x y -> 10 * x + y) 0
-- d2i4 :: [Integer] -> Integer
-- d2i4 = foldr (\ x y -> x + 10 * y) 0

-- ###########################################################################
-- 08- Choose an explanation for why the following definition of
-- sumsqreven is invalid:

-- sumsqreven = compose [sum, map (^2), filter even]

-- compose :: [a -> a] -> (a -> a)
-- compose = foldr (.) id

-- The definition of sumsqreven doesn't even typecheck.

-- ###########################################################################
-- 09- Choose the correct definition for the Prelude function curry ::
-- ((a, b) -> c) -> a -> b -> c , that converts a function that takes
-- its arguments as a pair into a function that takes its arguments
-- one at a time. For this exercise assume that bottom does not exist.

-- c1 f = \ x y -> f x y
-- c2 f = \ x y -> f
c3 f = \ x y -> f (x, y)
-- c4 f = \ (x, y) -> f x y

-- For additional understanding, try to experiment with undefined and
-- partial tuples, and see if you can spot any differences in
-- behaviour for the various implementations.

-- ###########################################################################
-- 10- Choose the definition for the Prelude function uncurry :: (a ->
-- b -> c) -> (a, b) -> c, that converts a function that takes its
-- arguments one at a time into a function that takes its arguments as
-- a pair. For this exercise assume that bottom does not exist.

u1 f = \ (x,y) -> f x y
u2 f = \ x y -> f (x,y)
u3 f = \ (x,y) -> f
u4 f = \ x y -> f

-- For additional understanding, try to experiment with undefined and
-- partial tuples, and see if you can spot any differences in
-- behaviour for the various implementations.

-- ###########################################################################
-- 11- Consider the following higher-order function unfold :: (b ->
-- Bool) -> (b -> a) -> (b -> b) -> b -> [a] that encapsulates a
-- simple pattern of recursion for producing a list.

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

--  The function unfold p h t x produces the empty list if the
--  predicate p x is True. Otherwise it produces a non-empty list by
--  applying the function h x to give the head of the generated list,
--  and the function t x to generate another seed that is recursively
--  processed by unfold to produce the tail of the generated list.

-- For example, the function int2bin, that converts a non-negative
-- integer into a binary number, with the least significant bit first,
-- can be defined as:

-- type Bit = Int
int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)

--  This function can be rewritten more compactly using unfold as
--  follows:

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

-- Next consider the function chop8 :: [Bit] -> [[Bit]] that takes a
-- list of bits and chops it into lists of at most eight bits
-- (assuming the list is finite, non-partial, and does not contain
-- bottom):

type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- Choose an implementation of chop8 using unfold.

-- cp81 = unfold [] (drop 8) (take 8)
cp82 = unfold null (take 8) (drop 8)
-- cp83 = unfold null (drop 8) (take 8)
-- cp84 = unfold (const False) (take 8 ) (drop 8)

-- ###########################################################################
-- 12- Following the previous question, choose an implementation of
-- map :: (a -> b) -> [a] -> [b] using unfold.

-- taking into account only finite, non-partial input lists with
-- non-bottom values, and where the mapping function does not return
-- bottom.

-- m1 f = unfold null (f) tail
-- m2 f = unfold null (f (head)) tail
m3 f = unfold null (f . head) tail
-- m4 f = unfold empty (f . head) tail

-- ###########################################################################
-- 13- Choose an implementation of the Prelude function iterate :: (a
-- -> a) -> a -> [a] using unfold.

i1 f = unfold (const False) id f
-- i2 f = unfold (const False) f f
-- i3 f = unfold (const True) id f
-- i4 f = unfold (const True) f f

-- ###########################################################################
-- 14- Assuming f, g and h are not bottom, the following equality
-- holds for all f, g and h of the correct type:

-- f . (g . h) = (f .g) . h

-- ###########################################################################
-- 15- Which of the following properties about lists is false:

-- [x] : xs = [x,xs]

-- ###########################################################################
-- 16- Which of the following properties about map and filter is true
-- for all f, g and p of the correct type:

-- filter p . filter p = filter p

-- ###########################################################################
-- 17- Which of the following is true for all non-bottom f, g and p of
-- the correct type, and finite, non-partial input lists xs that
-- contain no bottom values:

-- reverse (map f xs) = map f (reverse xs)

-- ###########################################################################
-- 18- Which of the following equations is true for all finite,
-- non-partial lists xs and ys, with non-bottom values:

-- reverse (xs ++ ys) = reverse ys ++ reverse xs

-- ###########################################################################
-- 19- Which of the following expressions produces a finite list:
-- take 10 [1..]

-- ###########################################################################
-- 20- Which of the following statements about the Prelude function
-- sum :: Num a => [a] -> a is false:

-- Sum is a high-order function

-- ###########################################################################
-- 21- Pick one of the wrong statements about the Prelude function map
-- :: (a -> b) -> [a] -> [b] :

-- map is a function with two arguments

-- ###########################################################################
-- 22- Which of the following statements about the Prelude function
-- foldr :: (a -> b -> b) -> b -> [a] -> b is false:

-- foldr is an overloaded function

-- ###########################################################################
-- 23- Which of the following statements about various Prelude
-- functions is true:

-- take is a polymorphic function

-- ###########################################################################
-- 24- Which equation defines a function f that is overloaded:

-- f x = x > 3

-- ###########################################################################
-- 25- Which of the following expressions is equal to [1, 2, 3, 4]:

-- take 4 (iterate (+1) 1)

-- ###########################################################################
-- 26- Evaluating takeWhile even [2, 4, 5, 6, 7, 8] gives:

-- [2,4]

-- ###########################################################################
-- 27- Evaluating zip [1, 2] ['a', 'b', 'c'] gives:

-- [(1,'a'), (2, 'b')]

-- ###########################################################################
-- 28- Evaluating foldr (-) 0 [1, 2, 3, 4] gives:

-- -2

-- ###########################################################################
-- 28- Evaluating filter even (map (+1) [1..5]) gives (Note: you can
-- copy and paste this expression directly from edX intro Hugs!):

-- [2,4,6]

-- ###########################################################################
-- 29- Which of the following expressions is equal to filter p (map f
-- xs), for all finite, non-partial lists xs with no bottom values,
-- and for all non-bottom f and p of the correct type:

-- [f x | x <- xs, p (f x)]

-- ###########################################################################
-- 30- After watching the jam session about Church Numerals, what
-- could be a possible implementation for exponentiation? (Note: you
-- have very many attempts to get this question correct)

--  cExp :: CNat -> CNat -> CNat

-- cExp (CNat a ) (CNat b) = CNat (b a)

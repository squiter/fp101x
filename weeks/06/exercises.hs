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

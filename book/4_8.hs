-- 01- Using library functions, define a function `halve::[a] ->
-- ([a],[a])` that splits an even-lengthed list into two halves.

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
           where
             n = length xs `div` 2

-- 02- Consider a function `safetail :: [a] -> [a]` thar behaves as
-- the library function `tail`, except that `safetail` maps the empty
-- list to itself, whereas `tail` produces an error in this case.
-- Define `safetail` using:

-- a) Conditional expressions:
stail1 :: [a] -> [a]
stail1 xs = if null xs then [] else tail xs

-- b) guarded equations:
stail2 :: [a] -> [a]
stail2 xs
  | null xs = []
  | otherwise = tail xs

-- c) Pattern matching:
stail3 :: [a] -> [a]
stail3 [] = []
stail3 (x:xs) = xs

-- 03- In similar way to ^, show how the logical disjunction operator
-- V can be defined in four different ways using pattern matching:

ou1 :: Bool -> Bool -> Bool
_ `ou1` True = True
True `ou1` _ = True
_ `ou1` _ = False

ou2 :: Bool -> Bool -> Bool
False `ou2` True = True
True `ou2` False = True
True `ou2` True = True
False `ou2` False = False

-- I don't know if the exercise want four differents `V`s or one `V`
-- defined four times like in `ou2` example

-- 04- Redefine the following version of the conjuction operator using
-- conditional expressions rather than pattern matching

-- True && True = True
-- _ && _ = False

and2 :: Bool -> Bool -> Bool
x `and2` y = if x && y then True else False

-- 05- Do te same for the following version, and note at the
-- difference of the number of conditional expression required

-- True && b = b
-- False && _ = False

and1 :: Bool -> Bool -> Bool
x `and1` y = if x then y else False

-- 06- Show how the curried function definition `mult x y z = x * y * z` can be understood in terms of lambda expressions


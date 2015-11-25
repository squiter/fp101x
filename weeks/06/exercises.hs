-- 00- Choose the equivalent of the following list comprehension [f x
-- | x <- xs, p x] expressed using higher-order functions.

-- map f (filter p xs)

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

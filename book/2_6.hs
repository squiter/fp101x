-- 01: Parenthesise the following arithmetic functions:
-- 2 ^ 3 * 4
-- 3 * 3 + 4 * 5
-- 2 + 3 * 4 ^ 5

-- ######################################################################

-- 02: Work through the examples using Hugs

-- ######################################################################

-- 03: The script below contains three syntactic errors. Correct the
-- errors and check in Hugs

-- N = a 'div' length xs
--     where
--         a = 10
--        xs = [1,2,3,4,5]

n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

-- ######################################################################

-- 04: Show how the library function `last` that selects the last
-- element of a non-empty list could be defined in terms of the
-- library functions introduced in this chapter. Can you think of
-- another possible definition?

-- I don't understand this question. :(

-- ######################################################################

-- 05: Show how the library function `init` that removes the last
-- element of a non-empty list could be defined in two different ways.
init' :: [a] -> [a]
init' xs = reverse (tail (reverse xs))

init'' :: [a] -> [a]
init'' xs = take (length xs - 1) xs

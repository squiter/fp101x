-- 00- Which of these expressions calculates the sum: 12+22+...+1002
-- of the first one hundred integer squares?

sum100 = sum [x^2 | x <- [1..100]]

-- 01- The library function replicate :: Int -> a -> [a] produces a
-- list of identical elements. Choose one possible implementation for
-- this function. For example:

-- replicate 3 True
--[True, True, True]

irep n a = [a | _ <- [1..n]]

-- 02- A triple (x, y, z) of positive integers is pythagorean if
-- x2+y2=z2. Choose the correct implementation for the function pyths
-- :: Int -> [(Int, Int, Int)] that returns the list of all
-- pythagorean triples whose components are at most a given limit.

-- pyths 10
--[(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2 == z^2]

-- 03- A positive integer is perfect if it equals the sum of its
-- factors, excluding the number itself. Choose the correct definition
-- of the function perfects :: Int -> [Int] that returns the list of
-- all perfect numbers up to a given limit.

-- Note: factors is not a library function but is defined in the
-- lecture.

-- perfects 500
-- [6, 28, 496]
factors n = [x | x <- [1..n], n `mod` x == 0]
perfects n = [x | x<-[1..n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num

-- 04- The following list comprehension:
[(x,y) | x <- [1,2,3], y <- [4,5,6]]

-- can be re-expressed using two or more comprehensions with single
-- generators. Choose the implementation that is equivalent to the one
-- above.

concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

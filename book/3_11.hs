-- 1- What are the types of the following values
-- [`a`, `b`, `c`]
[Char]
-- ('a', 'b', 'c')
(Char, Char, Char)
-- [(False, 'o'), (True, '1')]
[(Bool, Char)]
-- ([False, False], ['0', '1'])
([Bool],[Char])
-- [tail,init,reverse]
[a]

-- 2- What are the types of the following functions
second :: [a] -> a
second xs = head(tail xs)

--swap :: (a,a) -> (a,a)
swap :: (t1, t) -> (t, t1)
swap (x,y) = (y,x)

--pair :: a -> a -> (a,a)
pair :: t -> t1 -> (t,t1)
pair x y = (x,y)

double :: Num  a => a -> a
double x = x*2

--palindrome :: Ord => a -> [a] -> [a]
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f(f x)

-- 3- Test the answers in Hugs

-- 4- Why is not feasible in general for functions types to be an
-- instances of the Eq class? When is feasible? Hint: two functions of
-- the same type are equal if they always return equal results for
-- equal arguments

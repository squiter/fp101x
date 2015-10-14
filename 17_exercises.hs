-- Recursions
-- 01 replicate a value n times
ireplicate :: Int -> a -> [a]
ireplicate 0 _ = []
ireplicate n x = x : ireplicate (n -1) x

-- 02 select the nth element of a list
(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0 = x
(!!!) (_:xs) n = xs !!! (n-1)

-- Decide if a value is an element of a list
ielem :: Eq a => a -> [a] -> Bool
ielem a [] = False
ielem a (x:xs) = if a == x then True else ielem a xs

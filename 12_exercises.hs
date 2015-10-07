-- 01
-- a)
safetail_a xs = if null xs then [] else tail xs

-- b)
safetail_b xs | null xs = []
              | otherwise = tail xs

-- c)
safetail_c [] = []
safetail_c (_:xs) = xs

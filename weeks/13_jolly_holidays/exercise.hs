-- -- Happy new year!! (1/1 point)

-- As the last question of this MOOC, we present to you a juicy
-- problem about your two favorite functions in your favorite exercise
-- format!

-- Choose all correct definitions of foldl in terms of foldr

f1 :: Foldable t => (b -> a -> b) -> b -> t a -> b
f1 f a bs = foldr(\b -> \g -> (\a -> g (f a b))) id bs a

-- f2 :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- f2 f a bs = foldr (\a b -> f b a) a bs

f3 :: Foldable t => (b -> a -> b) -> b -> t a -> b
f3 f = flip $ foldr (\a b g -> (f g a)) id

-- f4 :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- f4 = foldr . flip

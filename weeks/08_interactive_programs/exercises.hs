import System.IO
import Data.Char

-- 01
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

-- 02

ps1 :: String -> IO ()
ps1 [] = putChar '\n'
ps1 xs = putStr' xs >> ps1 ""

ps2 :: String -> IO ()
ps2 [] = putChar '\n'
ps2 xs = putStr' xs >> putChar '\n'

ps3 :: String -> IO ()
ps3 [] = putChar '\n'
ps3 xs = putStr' xs >>= \x -> putChar '\n'

-- ps4 :: String -> IO ()
-- ps4 [] = putChar '\n'
-- ps4 xs = putStr' xs >> \x -> putChar '\n'

ps5 :: String -> IO ()
ps5 [] = putChar '\n'
ps5 xs = putStr' xs >> putStr' "\n"

ps6 :: String -> IO ()
ps6 [] = putChar '\n'
ps6 xs = putStr' xs >> ps6 "\n"

-- ps7 :: String -> IO ()
-- ps7 [] = return ""
-- ps7 xs = ps7 xs >> putStr' "\n"

-- ps8 :: String -> IO ()
-- ps8 [] = putChar "\n"
-- ps8 xs = putStr' xs >> putChar '\n'

-- 03
-- gl1 = g1 ""

-- g1 :: String -> IO String
-- g1 xs = do x <- getChar
--            case x of
--              ' ' -> return xs
--              '\n' -> return xs
--              _ -> g1 (xs ++ [x])

-- gl2 = g2 ""

-- g2 :: String -> IO String
-- g2 xs = do x <- getChar
--            case x of
--              '\n' -> return xs
--              _ -> g2 (x:xs)

getLine' = get []

get :: String -> IO String
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              _ -> get (xs ++ [x])

-- gl4 = g4 []

-- g4 :: String -> IO String
-- g4 xs = do x <- getChar
--            case x of
--              '\n' -> return (x:xs)
--              _ -> g4 (xs ++[x])

-- 04-
interact' :: (String -> String) -> IO ()
interact' f = do input <- getLine
                 putStrLn (f input)

-- i2 :: (String -> String) -> IO ()
-- i2 f = do input <- getLine
--           putStrLn input

-- -- i3 :: (String -> String) -> IO ()
-- -- i3 f = do input <- getChar
-- --           putStrLn (f input)

-- i4 :: (String -> String) -> IO ()
-- i4 f = do input <- getLine
--           putStr' (f input)

-- 05

-- s1 :: Monad m => [m a] -> m ()
-- s1 [] = return []
-- s1 (m:ms) = m >> \_ -> s1 ms

s2 :: Monad m => [m a] -> m ()
s2 [] = return ()
s2 (m:ms) = (foldl (>>) m ms) >> return ()

-- s3 :: Monad m => [m a] -> m ()
-- s3 ms = foldl (>>) (return ()) ms

s4 :: Monad m => [m a] -> m ()
s4 [] = return ()
s4 (m:ms) = m >> s4 ms

s5 :: Monad m => [m a] -> m ()
s5 [] = return ()
s5 (m:ms) = m >>= \_ -> s5 ms

-- s6 :: Monad m => [m a] -> m ()
-- s6 ms = foldr (>>=) (return ()) ms

sequence_' :: Monad m => [m a] -> m ()
sequence_' ms = foldr (>>) (return ()) ms

-- s8 :: Monad m => [m a] -> m ()
-- s8 ms = foldr (>>) (return []) ms

-- 06

ss1 :: Monad m => [m a] -> m [a]
ss1 [] = return []
ss1 (m:ms) = m >>=
             \a ->
             do as <- ss1 ms
                return (a:as)

-- ss2 :: Monad m => [m a] -> m [a]
-- ss2 ms = foldr func (return ()) ms
--   where
--     func :: (Monad m) => m a -> m [a] -> m [a]
--     func m acc = do x <- m
--                     xs <- acc
--                     return (x:xs)

-- ss3 :: Monad m => [m a] -> m [a]
-- ss3 ms = foldr func (return []) ms
--   where
--     func :: (Monad m) => m a -> m [a] -> m [a]
--     func m acc = m : acc

-- ss4 :: Monad m => [m a] -> m [a]
-- ss4 [] = return []
-- ss4 (m:ms) = return (a:as)
--   where
--     a <- m
--     as <- ss4 ms

ss5 :: Monad m => [m a] -> m [a]
ss5 ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = do x <- m
                    xs <- acc
                    return (x:xs)

-- ss6 :: Monad m => [m a] -> m [a]
-- ss6 [] = return []
-- ss6 (m:ms) = m >>
--              \a ->
--              do as <- ss6 ms
--                 return (a:as)

-- ss7 :: Monad m => [m a] -> m [a]
-- ss7 [] = return []
-- ss7 (m:ms) = m >>= \a ->
--   as <- ss7 ms
--   return (a:as)

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = do a <- m
                      as <- sequence' ms
                      return (a:as)


-- 07-

mm1 :: Monad m => (a -> m b) -> [a] -> m [b]
mm1 f as = sequence' (map f as)

mm2 :: Monad m => (a -> m b) -> [a] -> m [b]
mm2 f [] = return []
mm2 f (a:as) = f a >>= \b -> mm2 f as >>= \bs -> return (b:bs)

-- mm3 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mm3 f as = sequence_' (map f as)

-- mm4 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mm4 f [] = return []
-- mm4 f (a:as) = f a >> \b -> mm4 f as >> \bs -> return (b:bs)

-- mm5 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mm5 f [] = return []
-- mm5 f (a:as) =
--   do
--     f a -> b
--     mm5 f as -> bs
--     return (b:bs)

mm6 :: Monad m => (a -> m b) -> [a] -> m [b]
mm6 f [] = return []
mm6 f (a:as) = do b <- f a
                  bs <- mm6 f as
                  return (b:bs)

mm7 :: Monad m => (a -> m b) -> [a] -> m [b]
mm7 f [] = return []
mm7 f (a:as)
  = f a >>=
    \b ->
    do bs <- mm7 f as
       return (b:bs)

-- mm8 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mm8 f [] = return []
-- mm8 f (a:as)
--   = f a >>=
--     \b ->
--     do bs <- mm8 f as
--        return (bs ++ [b])


-- 08-
-- fm1 :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- fm1 _ [] = return []
-- fm1 p (x:xs)
--   = do flag <- p x
--        ys <- fm1 p xs
--        return (x:ys)

fm2 :: Monad m => (a -> m Bool) -> [a] -> m [a]
fm2 _ [] = return []
fm2 p (x:xs)
  = do flag <- p x
       ys <- fm2 p xs
       if flag then return (x:ys) else return ys

-- fm3 :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- fm3 _ [] = return []
-- fm3 p (x:xs)
--   = do ys <- fm3 p xs
--        if p x then return (x:ys) else return ys

-- fm4 :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- fm4 _ [] = return []
-- fm4 p (x:xs)
--   = do flag <- p x
--        ys <- fm4 p xs
--        if flag then return ys else return (x:ys)

-- 09-

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM _ acc [] = return acc
foldLeftM f acc (x:xs) = do a <- f acc x
                            foldLeftM f a xs

-- 10-
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM _ acc [] = return acc
foldRightM f acc (x:xs) = do a <- foldRightM f acc xs
                             f x a

-- 11 -
-- liftM (map toUpper) getLine

liftM1 :: Monad m => (a -> b) -> m a -> m b
liftM1 f m
  = do x <- m
       return (f x)

-- liftM2 :: Monad m => (a -> b) -> m a -> m b
-- liftM2 f m = m >>= \a -> f a

liftM3 :: Monad m => (a -> b) -> m a -> m b
liftM3 f m = m >>= \a -> return (f a)

-- liftM4 :: Monad m => (a -> b) -> m a -> m b
-- liftM4 f m = return (f m)

-- liftM5 :: Monad m => (a -> b) -> m a -> m b
-- liftM5 f m = m >>= \a -> m >>= \b -> return (f a)

-- liftM6 :: Monad m => (a -> b) -> m a -> m b
-- liftM6 f m = m >>= \a -> m >>= \b -> return (f b)

-- liftM7 :: Monad m => (a -> b) -> m a -> m b
-- liftM7 f m = mapM f [m]

-- liftM8 :: Monad m => (a -> b) -> m a -> m b
-- liftM8 f m = m >> \a -> return (f a)

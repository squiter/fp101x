import System.IO

-- Lecture 01

--getChar :: IO Char

--putChar :: Char -> IO ()

--return :: a -> IO a

-- getLine :: IO String
-- getLine = do x <- getChar
--              if x == '\n' then
--                return []
--              else
--                do xs <- getLine
--                   return (x:xs)

-- putStr :: String -> IO ()
-- putStr [] = return []
-- putStr (x:xs) = do putChar x
--                    putStr xs

-- putStrLn :: String -> IO ()
-- putStrLn xs = do putStr xs
--                  putChar '\n'

-- Lecture 02
hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)



getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
                  putStrLn "Wow, You got it!"
                else
                  do putStrLn (diff word xs)
                     guess word
diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x<-xs]

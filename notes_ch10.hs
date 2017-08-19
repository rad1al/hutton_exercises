{- Chapter 10 :: Interactive Programming -}

{-

Note: Whitespace seems to be important. Lines should 
be vertically aligned with the word after the first 'do'.

Already implemented in ghci:


-}



import System.IO
import Data.Char (isDigit, digitToInt)

--------- Hangman Game ---------

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word


-- get secret word from user and display them as
-- '-' characters.

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
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it!!"
               else
                  do putStrLn (match word guess)
                     play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)


{- 

Upon entering the letters 'a','b','c':

> act
abd('a','c')

-}


strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters" 


{-

> strlen
Enter a string: hello
The string has 5 characters

-}

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

getLine' :: IO String
getLine' = do x <-getChar
              if x == '\n' then
                 return []
              else
                 do xs <- getLine'
                    return (x:xs)

{-

> getLine'
hello world
"hello world"

-}

--------- Hangman Game ---------

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

{-

all (> 5) [6,7,8]
= True

-}


finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

{-

initial board : [5,4,3,2,1]
valid initial 1 3 
= [5,4,3,2,1] !! (1-1) >= 3
= [5,4,3,2,1] !! 0 >= 3
= 5 >= 3
= True


-}

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
                      where update r n = if r == row 
                                            then n-num 
                                         else n


{-

move initial 1 3
= [2,4,3,2,1]

-}

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

{-

> putRow 2 2
2: * * 

> putRow 1 5
1: * * * * * 

-}

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e
newline :: IO ()
newline = putChar '\n'


getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit."
                           getDigit prompt

{-

> getDigit "Enter a row number: "
Enter a row number: a
ERROR: Invalid digit.
Enter a row number: 2
2

-}


play' :: Board -> Int -> IO ()
play' board player =
   do newline
      putBoard board
      if finished board then
         do newline
            putStr "Player "
            putStr (show (next player))
            putStrLn " wins!!"
      else
         do newline
            putStr "Player "
            putStrLn (show player)
            row <- getDigit "Enter a row number: "
            num <- getDigit "Stars to remove: "
            if valid board row num then
               play' (move board row num) (next player)
            else
               do newline
                  putStrLn "ERROR: Invalid move"
                  play' board player

nim :: IO ()
nim = play' initial 1

--------- Game of Life ---------

-- clear screen --
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type Board' = [Pos]

glider :: Board'
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]


{-

The library function sequence_ :: [IO a] -> IO ()
performs a list of actions in sequence, discarding their 
result values and returning no result.

-}

showcells :: Board' -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board' -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board' -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                          (x+1,y-1), (x-1,y),
                          (x+1,y), (x-1,y+1),
                          (x,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, 
              ((y-1) `mod` height) + 1)

liveneighbs :: Board' -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board' -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board' -> [Pos]
births b = [(x,y) | x <- [1..width],
                    y <- [1..height],
                    isEmpty b (x,y),
                    liveneighbs b (x,y) == 3]


rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)


{-

The next generation of a board can be produced by appending 
the list of survivors and the list of new births.

-}

nextgen :: Board' -> Board'
nextgen b = survivors b ++ births b

life :: Board' -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)


{- slow the game down to a reasonable speed by 
performing a given number of dummy actions -}

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

{-

To run game of life implementation:

> life glider

To stop process: ctrl + c

-}















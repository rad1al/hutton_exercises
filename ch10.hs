{- Chapter 10.10 Exercises -}

import System.IO
import Data.Char (isDigit, digitToInt)

{- 1. -}

putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- modification of the Nim game for exercises 2 & 3 --

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

larger_board :: Board
larger_board = [7,6,5,4,3,2,1]

smaller_board :: Board
smaller_board = [3,2,1]

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

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

-- Original putBoard implementation --

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

{- 2. -}

-- putBoard' displays nim boards of any size 
-- with putBoardHeler auxiliary function.

putBoard' :: Board -> IO ()
putBoard' = putBoardHelper 1

putBoardHelper :: Int -> Board -> IO ()
putBoardHelper row []     = return ()
putBoardHelper row (x:xs) = do putRow row x
                               putBoardHelper (row+1) xs

{- 3. -}

-- putBoard implementation using sequence_ 
-- and a list comprehension. 

putBoard'' :: Board -> IO ()
putBoard'' xs = sequence_ [putRow a b | (a,b) <- zip [1..] xs]

newline :: IO ()
newline = putChar '\n'


-- needed for exercise 4: 

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit."
                           getDigit prompt

play' :: Board -> Int -> IO ()
play' board player =
   do newline
      putBoard' board
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
nim = play' smaller_board 1

-- {- 4. -}

adder_helper :: Int -> Int -> IO Int
adder_helper a b = do x <- getDigit ""
                      if b == 1 then
                          return (a+x)
                      else
                          adder_helper (a+x) (b-1)


{-

> adder_helper 0 2
3 <- user input
4 <- user input
7

trace of the above:

adder_helper 0 2 

=   take '3' from input
    convert to Int and assign to x
    return adder_helper (0+3) (1)

=   take '4' from input
    convert to Int and assign to x
    return (3+4)

=   7

example of catching non-digits:

> adder_helper 0 2
a
ERROR: Invalid digit.
e
ERROR: Invalid digit.
1
9

-}


adder :: IO ()
adder = do n <- getDigit "How many numbers?: "
           if n < 1 then
               do putStrLn "ERROR: there must be more than 0 numbers!"
                  adder -- run again if invalid input is detected.
           else
               do total <- adder_helper 0 n
                  putStr "the total is "
                  putStrLn (show total) -- convert Int to String with show

{- 5. -}

{-

From Peter Wortmann @ StackOverflow:

By definition sequence [[1,2],[3,4]] is the same as:

do x <- [1,2]
   y <- [3,4]
   return [x,y]

> sequence [[1,2],[3,4]]
[[1,3],[1,4],[2,3],[2,4]]

"First a choice between 1 and 2, then a choice between 3 and 4". 
The list monad will now accumulate all possible outcomes - 
hence the answer [[1,3],[1,4],[2,3],[2,4]]

Also seems like the Cartesian Product.

-}

-- collect all the numbers from input into a list.

adder_helper' :: Int -> IO [Int]
adder_helper' a = sequence (replicate a (getDigit ""))

{-

> adder_helper' 3
1
2
3
[1,2,3]

-}

-- implementation of adder with sequence_

adder' :: IO ()
adder' = do n <- getDigit "How many numbers?: "
            if n < 1 then
               do putStrLn "ERROR: there must be more than 0 numbers!"
                  adder'
            else
               do nums <- adder_helper' n
                  putStr "the total is "
                  putStrLn (show $ sum nums) -- convert Int to String with show


{-

> adder'
How many numbers?: 3
3
4
5
the total is 12

-}


-- {- 6. -}

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine_helper :: String -> IO String
readLine_helper xs = do x <- getCh
                        if x == '\n' then
                           do putChar x
                              return xs
                        else if x == '\DEL' then
                           do putChar '\b'
                              putChar ' '
                              putChar '\b'
                              readLine_helper $ remove_char xs
                        else
                           do putChar x
                              readLine_helper (xs ++ [x])

-- remove a character from the end of a list safely. 
-- otherwise empty list exception will occur.

remove_char :: [Char] -> [Char]
remove_char [] = []
remove_char [x] = []
remove_char (x:xs) = init (x:xs)

readLine :: IO String
readLine = readLine_helper []











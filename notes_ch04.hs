{- Chapter 4 :: Defining functions -}

{-
------------------ New from old -----------------

New functions can simply be defined by combining one or more 
existing functions.

-}

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs = (take n xs, drop n xs)

recip' :: Fractional a => a -> a
recip' n = 1/n

abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

signum' :: Int -> Int
signum' n = if n < 0 
               then -1 
            else
               if n == 0 then 0 else 1

------------------ Guarded equations -----------------

signum'' :: Int -> Int
signum'' n | n < 0     = -1
           | n == 0    = 0
           | otherwise = 1


test :: [Char] -> Bool
test ['a',_,_] = True
test _         = False

{-

> test ['a','b','c']
True

> test ['d','b','c']
False

-}

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

const' :: a -> b -> a
const' x _ = x

const'' :: a -> b -> a
const'' x = \_ ->  x

odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..n-1]

{-

> odds 5
[1,3,5,7,9]


------------------ Operators -----------------

Functions such as + that are written between two arguments are called
operators.

(+)   = addition function
(1+)  = successor function
(1/y) = reciprocation function
(*2)  = doubling function
(/2)  = halving function

-}

sum' :: [Int] -> Int
sum' = foldl (+) 0

{-

> sum' [1,2,3,4]
10

-}


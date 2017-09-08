{- Chapter 3 :: Types and classes -}

{-

False :: Bool
True :: Bool
not :: Bool -> Bool

------------------ Basic types ------------------

Bool    = logical values
Char    = single characters
String  = strings of characters
Int     = fixed-precision integers
Integer = arbitrary-precision integers 
Float   = single-precision floating-point numbers
Double  = double-precision floating-point numbers

[False, True, False] :: [Bool]
['a','b','c','d'] :: [Char]
["One","Two","Three"] :: [String]

[['a','b'],['c','d','e']] :: [[Char]]

-}

add :: (Int, Int) -> Int
add (x,y) = x+y

zeroto :: Int -> [Int]
zeroto n = [0..n]

------------------ Curried function ------------------

add' :: Int -> (Int -> Int)
add' x y = x+y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

{-

mult x y z

means:

((mult x) y) z

------------------ Basic classes ------------------

Eq         = equality types
Ord        = ordered types
Show       = showable types
Read       = readable types
Num        = numeric types
Integral   = integral types
Fractional = fractional types

-}
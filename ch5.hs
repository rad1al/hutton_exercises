-- 5.7 Exercises.

import Data.Char

{- 1. -}

sum_of_squares = sum [x^2 | x <- [1..100]] 

{- 2. -}

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

{- 3. -}

square :: Int -> [(Int, Int)]
square x = [(a, b) | (a, b) <- grid x x, a /= b]

{- 4. -}

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

{- 5. -}

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

{- 6. -}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

{- 7. -}

combined = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]

{- 8. -}

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])


{- 9. -}

scalar :: [Int] -> [Int] -> Int
scalar xs ys = sum [x * y | (x,y) <- zip xs ys]

{- 10. -}

-- To be implemented.












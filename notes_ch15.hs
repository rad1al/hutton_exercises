{- Chapter 15 :: Lazy evaluation -}

inc :: Int -> Int
inc n = n + 1


mult :: (Int,Int) -> Int
mult (x,y) = x * y


{-

> mult(1+2,2+3)
15

-}

mult' :: Int -> Int -> Int
mult' x = \y -> x * y

{-

> mult' (1+2) (2+3)
15

-}

inf :: Int
inf = 1 + inf

{-

> fst (0, inf)
0

-}

square :: Int -> Int
square n = n * n

ones :: [Int]
ones = 1 : ones

{-

> head ones
1

> take 3 ones
[1,1,1]

from standard prelude:

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs


replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

> filter (<= 5) [1..]
[1,2,3,4,5.. (will loop endlessly)

> takeWhile (<= 5) [1..]
[1,2,3,4,5]
-}

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

{-

> take 10 primes
[2,3,5,7,11,13,17,19,23,29]

> takeWhile (< 10) primes
[2,3,5,7]

------------------ Strict Application ---------------

> square (1+2)
9

> square $! (1+2)
9

square $! (1+2)
= square $! 3
= square 3
= 3 * 3
= 9

-}

sumwith :: Int -> [Int] -> Int
sumwith v [] = v
sumwith v (x:xs) = sumwith (v+x) xs

{-

sumwith 0 [1,2,3]
= sumwith (0+1) [2,3]
= sumwith ((0+1)+2) [3]
= sumwith (((0+1)+2)+3) []
= sumwith (((0+1)+2)+3) []
= ((0+1)+2)+3
= (1+2) + 3
= 3 + 3
= 6

-}

sumwith' :: Int -> [Int] -> Int
sumwith' v [] = v
sumwith' v (x:xs) = (sumwith' $! (v+x)) xs

{-

sumwith' 0 [1,2,3]
= (sumwith' $! (0+1)) [2,3]
= (sumwith' $! 1) [2,3]
= sumwith' 1 [2,3]
= (sumwith' $! (1,2)) [3]
= (sumwith' $! 3) [3]
= sumwith' 3 [3]
= (sumwith' $! (3+3)) []
= (sumwith' $! 6) []
= sumwith' 6 []
= 6

-}

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = ((foldl' f) $! (f v x)) xs

sumwith'' :: Int -> [Int] -> Int
sumwith'' = foldl' (+)

{-

> sumwith'' 0 [1,2,3]
6

-}





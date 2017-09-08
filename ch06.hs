{- 6.8 Exercises -}

{- 1. -}

-- factorial function that prohibits negative arguments. 

fac'' :: Int -> Int
fac'' n | n < 0  = error "input is negative"
        | n == 0 = 1
        | otherwise = n * fac'' (n-1)


{- 2. -} 

sumdown :: [Int] -> Int
sumdown []   = 0
sumdown (x:xs) = x + sumdown xs 

{- 3. -}

(^*) :: Int -> Int -> Int
x ^* 0 = 1
x ^* n = x * (x ^* (n-1))

{- 4. -}

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | otherwise = if x < y
                          then euclid x (y-x)
                         else euclid y (x-y) 


{- 5. 

    length [1,2,3]
=   1 + (length [2,3])
=   1 + (1 + (length [3]))
=   1 + (1 + (1 + (length [])))
=   1 + 1 + 1 + 0
=   3

    drop 3 [1,2,3,4,5]
=   drop 2 [2,3,4,5]
=   drop 1 [3,4,5]
=   drop 0 [4,5]
=   [4,5]

    init [1,2,3]
=   1 : init [2,3]
=   1 : init 2 : init [3]
=   1 : init 2 : []
=   [1,2]

-}

{- 6. -}

-- a. Decide if all logical values in a list are True:
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and xs)

-- b. Concatenate a list of lists:
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ (concat' xs)

-- c. Produce a list with n identical elements:
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : (replicate' (n-1) x)

-- d. Select the nth element of a list:
(!!*) :: [a] -> Int -> a
(x:xs) !!* n | n  <   0     = error "negative index"
             | n  ==  0     = x
             | otherwise    = xs !!* (n-1)

-- e. Decide if a value is an element of a list:

-- elem' :: Eq a => a -> [a] -> Bool
-- elem' a [] = False
-- elem' a (x:xs) | x == a = True
--                | otherwise = False || (elem' a xs)
--
-- Rewritten as:

elem' :: Eq a => a -> [a] -> Bool
elem' a (x:xs) | (x:xs) == []  = False
               | x      == a   = True
               | otherwise     = False || (elem' a xs)


{- 7. -}

merge :: Ord a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

{- 8. -}

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = splitAt (length xs `div` 2) xs

-- msort :: Ord a => [a] -> [a]
-- msort [] = []
-- msort [x] = [x]
-- msort xs = merge (msort (fst $ halve xs)) (msort (snd $ halve xs))
--
-- Rewritten as:

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort a) (msort b)
           where (a, b) = halve xs

{- 9. -}

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs


take' :: Int -> [a] -> [a]
take' 0 xs     = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' (x:xs) | null xs  = x
             | otherwise = last' xs






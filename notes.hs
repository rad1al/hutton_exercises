{- Chapter 6 -}

fac :: Int -> Int
fac n = product [1..n]

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac' (n-1)

-- append operator
(++*) :: [a] -> [a] -> [a]
[]     ++* ys = ys
(x:xs) ++* ys = x : (xs ++* ys)

{-
    [1,2,3] ++* [4,5]
=   1 : ([2,3] ++* [4,5])
=   1 : (2: ([3] ++* [4,5]))
=   1 : (2: (3 : ([] ++* [4,5])))
=   1 : (2: (3 : ([4,5])))
=   [1,2,3,4,5]

-}

-- inserting a new element of any ordered type into
-- a sorted list.
insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys


{- 
    insert 3 [1,2,4,5]
=   1 : insert 3 [2,4,5]
=   1 : 2 : insert 3 [4,5]
=   1 : 2 : 3 : [4,5]
=   [1,2,3,4,5]
    
-}

-- insertion sort

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

{- 
    isort [3,2,1,4]
=   insert 3 (insert 2 (insert 1 (insert 4 [])))
=   insert 3 (insert 2 (insert 1 [4]))
=   insert 3 (insert 2 [1,4])
=   insert 3 ([1,2,4])
=   [1,2,3,4]

-}

-- zip : take two lists and produce a list of pairs.
zip' :: [a] -> [b] -> [(a,b)]
zip' []     _      = []
zip' _      []     = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n-1) xs

{- Multiple recursion -}

fib'' :: Int -> Int
fib'' 0 = 0
fib'' 1 = 1
fib'' n = fib'' (n-2) + fib'' (n-1)

{- Mutual recursion -}

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

{-

    evens "abcde"
=   'a' : odds "bcde"
=   'a' : evens "cde"
=   'a' : 'c' : odds "de"
=   'a' : 'c' : evens "e"
=   'a' : 'c' : 'e' : odds []
=   'ace'

-}


drop'' :: Int -> [a] -> [a]
drop'' 0 xs = xs
drop'' _ [] = []
drop'' n (x:xs) = drop'' (n-1) xs

init' :: [a] -> [a]
init' (x:xs) | null xs   = []
             | otherwise = x : init' xs

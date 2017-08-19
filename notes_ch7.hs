import Data.Char
import Data.List (sort)

{- Chapter 7 :: Higher-order functions -}

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- map defined by recursion

map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

-- filter defined by recursion

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p [] = []
filter'' p (x:xs) | p x       = x : filter'' p xs
                | otherwise = filter'' p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

-- foldr function
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

{-

(foldr (+) 0) [1,2,3]

=   1 : (2 : (3 : []))
=   1 + (2 + (3 + 0))

Takeaway: foldr can be thought of replacing each cons
operator in a list by the function f and the empty list
at the end by the value v.

-}

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product xs

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

or' :: [Bool] -> Bool
or' []     = False
or' (x:xs) = x || or' xs

or'' :: [Bool] -> Bool
or'' = foldr (||) False

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

length' :: [a] -> Int
length' = foldr (\_ n -> 1+n) 0

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldr snoc [] 

{-

    reverse [1,2,3]
=   1 : (2 : (3 : []))
=   1 snoc (2 snoc (3 snoc []))
=   (2 snoc (3 snoc [])) ++ [1]
=   ((3 snoc []) ++ [2]) ++ [1]
=   (([] ++ [3]) ++ [2]) ++ [1]

-}

-- foldl function

sum''' :: Num a => [a] -> a
sum''' = sum' 0
         where
            sum' v [] = v
            sum' v (x:xs) = sum' (v+x) xs

{- 

    sum [1,2,3]
=   sum' 0 [1,2,3]
=   sum' (0+1) [2,3]
=   sum' ((0+1)+2) [3]
=   sum' (((0+1)+2)+3) []
=   sum' ((0+1)+2)+3
=   6

-}

length'' :: [a] -> Int
length'' = foldl (\n _ -> n+1) 0

-- length'' [1,2,3] = ((0 + 1) + 1) + 1 = 3

reverse''' :: [a] -> [a]
reverse''' = foldl (\xs x -> x:xs) []

{- 

swap_cons = (\xs x -> x:xs)
swap_cons [2,3] 1 == [1,2,3]

    reverse''' [1,2,3]
=   ((([] swap_cons 1) swap_cons 2) swap_cons 3)
=   (([1] swap_cons 2) swap_cons 3)
=   ([2,1] swap_cons 3)
=   [3,2,1]

-}

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

-- composition operator

(.*) :: (b -> c) -> (a -> b) -> (a -> c)
f .* g = \x -> f (g x)

{-

not . even $ 10 == False
not . not . even $ 10 == True

-}

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

{- 

compose [\x -> x^2, \y -> y*5] 4 == 400 

-}

-- bit conversion

type Bit = Int

{- 

iterate produces an infinite list by applying
a function on a value - i.e.:

iterate (*10) 1 = [1, 10, 100..]
take 5 $ iterate (*2) 1 == [1,2,4,8,16] 

-}

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
                  where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

{- 

bin2int' [1,0,1,1] == 13
int2bin  13        == [1,0,1,1]
make8    [1,1,0,1] == [1,1,0,1,0,0,0,0]

-}

-- convert each letter to bit representation and 
-- concatenate the output lists into one list.

encode' :: String -> [Bit]
encode' = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int') . chop8

{- 

encode' "abc"
= [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

chop8 [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
= [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]

decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
= "abc"

-}

transmit :: String -> String
transmit = decode . channel . encode'

channel :: [Bit] -> [Bit]
channel = id

{- 

transmit "higher-order functions are easy"
= "higher-order functions are easy

-}


-- voting algorithms

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

{- 
count "Red" votes == 2
-}

rmdupes :: Eq a => [a] -> [a]
rmdupes [] = []
rmdupes (x:xs) = x : rmdupes (filter (/= x) xs) 


{-
Incorrect definition of rmdups, check Errata on
http://www.cs.nott.ac.uk/~pszgmh/pih-errata.html

rmdupes :: Eq a => [a] -> [a]
rmdupes [] = []
rmdupes (x:xs) = x : filter (/= x) (rmdupes xs) 

rmdupes [1,3,1,3,2]
= 1 : rmdupes (/= 1) [3,1,3,2]
= 1 : filter (/= 1) (rmdupes [3,1,3,2])
= 1 : filter (/= 1) (3 : filter (/= 3) (rmdupes [1,3,2]))
= 1 : filter (/= 1) (3 : filter (/= 3) (1 : filter (/= 1) (rmdupes [3,2])))
= 1 : filter (/= 1) (3 : filter (/= 3) (1 : filter (/= 1) (3 : filter (/= 3) (rmdupes [2]))))
= 1 : filter (/= 1) (3 : filter (/= 3) (1 : filter (/= 1) (3 : filter (/= 3) (2 : filter (/= 2) (rmdupes [])))))
= 1 : filter (/= 1) (3 : filter (/= 3) (1 : filter (/= 1) (3 : filter (/= 3) (2 : filter (/= 2) []))))
= 1 : filter (/= 1) (3 : filter (/= 3) (1 : filter (/= 1) [3,2]))
= 1 : filter (/= 1) (3 : filter (/= 3) [1,3,2])
= 1 : filter (/= 1) (3 : [1,2])
= 1 : filter (/= 1) [3,1,2]
= 1 : [3,2]
= [1,3,2]

rmdupes votes
= ["Red","Blue","Green"]

-}


result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdupes vs]



winner :: Ord a => [a] -> a
winner = snd . last . result

{- 

result votes
= [(1,"Green"),(2,"Red"),(3,"Blue")]

winner votes
= "Blue"

-}

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Green"],
           ["Blue"],
           ["Green","Red","Blue"],
           ["Blue","Green","Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- case mechanism allows for pattern matching in the 
-- body of a definition.

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c] -> c
                (c:cs) -> winner' (elim c bs)

{- 

rank ballots
= ["Red", "Blue", "Green"]

winner' ballots
= "Green"

-}

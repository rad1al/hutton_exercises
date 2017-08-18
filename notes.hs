 Chapter 6 :: Recursive Functions -}

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

{- Chapter 9 :: Declaring types and classes -}

{-

type String = [Char]    

Not permitted:

type Tree = (Int, [Tree])

-}

type Pos = (Int,Int)

type Trans = Pos -> Pos -- Transformation function

type Pair a = (a, a)

type Assoc k v = [(k, v)]

-- return the first value that is associated
-- with a given key in a table.

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

{-

find 1 [(1,3),(2,3),(4,5)]
= 3

-}

-- data Bool = False | True

data Move = North | South | East | West deriving Show

{-

The phrase 'deriving show' added to the end of the data 
declaration will ensure the system can display values of
the new type.

-}

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East

{-

moves [North, East] (0,0)
= (1,1)

-}

-- data Shape = Circle Float | Rect Float Float deriving
data Shape = Circle Float | Rect Float Float deriving (Eq, Ord)

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

{-

safediv 10 2
= Just 5

safediv 10 0
= Nothing


-}

-- type Nat = Int
-- newtype Nat = N Int

-- Succ the is successor function (1+)

data Nat = Zero | Succ Nat deriving Show

{-

Succ (Succ (Succ Zero))
= 1 + (1 + (1 + 0)) 
= 3

-}

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n


int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

{-

nat2int $ Succ (Succ (Succ Zero))
= 3

int2nat 3
= Succ (Succ (Succ Zero))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

-}

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

{-
add (Succ (Succ Zero)) (Succ Zero)
= Succ (add (Succ Zero) (Succ Zero))
= Succ (Succ (add Zero (Succ Zero)))
= Succ (Succ (Succ Zero))
-}



data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

{-
len $ Cons 20 (Cons 10 (Nil))
= 2
-}

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

{-

occurs 3 t
= True

occurs 9 t
= True

occurs 10 t
= False

-}

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

{-

flatten t
= [1,3,4,5,6,7,9]

-}

-- occurs function for search tree - much more efficient

occurs' :: Ord a => a -> Tree a  -> Bool
occurs' x (Leaf y)                 = x == y
occurs' x (Node l y r) | x == y    = True
                       | x < y     = occurs' x l
                       | otherwise = occurs' x r


{-

occurs' 3 t
= True

occurs' 9 t
= True

occurs' 10 t
= False

data Tree a = Leaf a | Node (Tree a) (Tree a)

data Tree a = Leaf | Node (Tree a) a (Tree a)

data Tree a b = Leaf a | Node (Tree a b) a (Tree a a)

data Tree a = Node a [Tree a]

The appropriate form of the tree depends on the situation.

-}


-- class and instance declarations



{-

class Eq' a where
    (==*), (/=*) :: a -> a -> Bool
    x /=* y = not (x ==* y)

instance Eq' Bool where
    False ==* False = True
    True  ==* True  = True
    _     ==* _     = False

class Eq a => Ord a where
    (<), (<=), (>), (>=) :: a -> a -> Bool
    min, max             :: a -> a -> a
    
    min x y | x <= y = x
            | otherwise = y
    
    min x y | x <= y = x
            | otherwise = y

instance Ord Bool where
    False < True = True
    _     < _    = False
    
    b <= c = (b < c) || (b == c)
    b > c = c < b
    b >= c = c <= b

-}

-- derived instances

{-

data Bool = False | True
                deriving (Eq, Ord, Show, Read)


data Shape = Circle Float | Rect Float Float deriving Ord
-}

-- instance Ord Shape where
    -- area (Rect x y) < area (Rect x y) = True


{- 

Rect 1.0 4.0 < Rect 2.0 3.0
= True

-}

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


type Subst = Assoc Char Bool

{-

the substitution [('A', False), ('B', True)]
assigns the variable A to False and B to True.

-}

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

{-

eval [('A',False),('B',False)] p1
= False

eval [('A',False),('B',False)] p2
= True

-}

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

{- 

vars p1
= "AA"

vars p2
= "ABA"

vars p3
= "AAB"

vars p4
= "AABB"

-- implementation of bools with int2bin from chapter 7

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
          where 
            range = [0..(2^n)-1]
            make n bs = take n (bs ++ repeat 0)
            conv 0 = False
            conv 1 = True

map (3:) [[1],[20]]
= [[3,1],[3,20]]

-}



bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
            where bss = bools (n-1)

-- Defined earlier in chapter 7
-- rmdupes :: Eq a => [a] -> [a]
-- rmdupes [] = []
-- rmdupes (x:xs) = x : rmdupes (filter (/= x) xs) 

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
             where vs = rmdupes (vars p)
{-

bools 3
=   [[False,False,False],
    [False,False,True],
    [False,True,False],
    [False,True,True],
    [True,False,False],
    [True,False,True],
    [True,True,False],
    [True,True,True]]

bools 2
=   [[False,False],
    [False,True],
    [True,False],
    [True,True]]

substs p2
= map (zip (rmdupes (vars p2))) (bools (length (rmdupes (vars p2))))
= map (zip (rmdupes "ABA")) (bools (length (rmdupes "ABA")))
= map (zip (rmdupes "ABA")) (bools (length "AA"))
= map (zip "AB") (bools 2)
= map (zip "AB") [[False,False],[False,True],[True,False],[True,True]]
= [[('A',False),('B',False)],[('A',False),('B',True)],[('A',True),('B',False)],[('A',True),('B',True)]]

-}

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

{-

isTaut p1
= False

isTaut p2
= True

isTaut p3
= False

isTaut p4
= True

-}

-- Abstract machine

data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

{- 

value (Add (Add (Val 2) (Val 3)) (Val 4))
= value (Add (value (Val 2) (Val 3))) + value (Val 4)
= value (Add (Val 2) (Val 3)) + value (Val 4)
= value (value (Val 2) + value (Val 3)) + value (Val 4)
= (value (Val 2) + value (Val 3)) + value (Val 4)
= 2 + value (Val 3) + value (Val 4)
= (2 + 3) + value (Val 4)
= 5 + value (Val 4)
= 5 + 4
= 9

The definition of the value function does not specify if the left 
argument of an addition should be evaluated before the right.

Basically, the order of evalution is determined by Haskell though
control structures can be made explict by defining an abstract 
machine for expressions, which specifies the step-by-step process
of their evaluation.

-}

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval' :: Expr -> Cont -> Int
eval' (Val n)   c = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c)  m = exec c (n + m)

value' :: Expr -> Int
value' e = eval' e []

{-

value' (Add (Add (Val 2) (Val 3)) (Val 4))
= eval' (Add (Add (Val 2) (Val 3)) (Val 4)) []
= eval' (Add (Val 2) (Val 3)) (EVAL (Val 4) : [])
= eval' (Add (Val 2) (Val 3)) [EVAL (Val 4)]
= eval' (Val 2) (EVAL (Val 3): [EVAL (Val 4)])
= eval' (Val 2) [EVAL (Val 3), EVAL (Val 4)]
= exec  [EVAL (Val 3), EVAL (Val 4)] 2
= exec  [EVAL (Val 3), EVAL (Val 4)] 2
= eval' (Val 3) (ADD 2 : [EVAL (Val 4)])
= eval' (Val 3) [ADD 2, EVAL (Val 4)]
= exec  [ADD 2, EVAL (Val 4)] 3
= exec  [EVAL (Val 4)] (2 + 3)
= exec  [EVAL (Val 4)] 5
= eval' (Val 4) (ADD 5 : [])
= eval' (Val 4) [ADD 5]
= exec  [ADD 5] 4
= exec  [] (5 + 4)
= exec  [] 9
= 9
= 9


{- Chapter 8 :: Declaring types and classes -}

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
data Shape = Circle Float | Rect Float Float deriving (Eq, Show)

instance Ord Shape where
   (Rect x y) <= (Rect m n) = area (Rect x y) <= area (Rect m n)
   (Rect x y) > (Rect m n) = area (Rect x y) > area (Rect m n)

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
--   (Rect x y) <= (Rect m n) = area (Rect x y) <= area (Rect m n)
--   (Rect x y) > (Rect m n) = area (Rect x y) > area (Rect m n)


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
rmdupes :: Eq a => [a] -> [a]
rmdupes [] = []
rmdupes (x:xs) = x : rmdupes (filter (/= x) xs) 

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

-}
{- Chapter 8.9 Exercises -}

{- 1. -}

data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


{-
add (Succ (Succ Zero)) (Succ Zero)
= Succ (add (Succ Zero) (Succ Zero))
= Succ (Succ (add Zero (Succ Zero)))
= Succ (Succ (Succ Zero))
-}

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

{-

2 x 1:

mult (Succ (Succ Zero)) (Succ Zero) 
= add (Succ (Succ Zero)) (mult (Succ (Succ Zero)) Zero)
= add (Succ (Succ Zero)) Zero
= Succ (add (Succ Zero) Zero)
= Succ (Succ (add Zero Zero))
= Succ (Succ (Zero))
= Succ (Succ Zero)
= 2


2 x 3:

2 x (1 + 2)
= 2 + (2 * 2)
= 2 + (2 + (2 * 1)
= 2 + (2 + (2 + (2 * 0))
= 2 + (2 + (2 + 0))
= 6

mult (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
= Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
= 6

-}

{- 2. 

:t compare
> compare :: Ord a => a -> a -> Ordering

compare 3 5
= LT

-}

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

-- occurs :: Eq a => a -> Tree a -> Bool
-- occurs x (Leaf y)     = x == y
-- occurs x (Node l y r) = x == y || occurs x l || occurs x r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                            LT -> occurs x l
                            EQ -> True
                            GT -> occurs x r
{- 

It is more efficient because it only searches the side of the tree 
that is likely to have the value.

-}


{- 3. -}

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show


countLeaves :: Tree' a -> Int
countLeaves (Leaf' a) = 1
countLeaves (Node' l r) = countLeaves l + countLeaves r

t' :: Tree' Int
t' = Node' (Node' (Leaf' 3) (Leaf' 4)) 
           (Node' (Leaf' 2) (Node' (Leaf' 5) (Leaf' 1)))

-- a tree is balanced if the leaves in the left and right subtree
-- differs by most one and also the subtrees themselves are balanced.

balanced :: Tree' a -> Bool
balanced (Leaf' a) = True
balanced (Node' l r) = (abs $ (countLeaves l) - (countLeaves r)) <= 1 
                       && balanced l 
                       && balanced r


{- 4. -}

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs  = Node' (balance a) (balance b) 
            where (a,b) = halve xs

{- 

balance [1,2,3,4,5]
= Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Node' (Leaf' 4) (Leaf' 5)))

balance [3, 2, 4]
= Node' (Leaf' 3) (Node' (Leaf' 2) (Leaf' 4))

-}


{- 5. -}

data Expr = Val Int | Add Expr Expr | Mult Expr Expr



folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y) 

{-

folde (\x -> x+10) (\x y -> x + y) (Val 2) 
= 12

folde (\x -> x+10) (\x y -> x + y) (Add (Val 2) (Val 3)) 
= 25

-}

{- 6. -}

eval' :: Expr -> Int
eval' = folde id (+)


size :: Expr -> Int
size = folde (\x -> 1) (+)

{-

eval' (Add (Val 3) (Val 4)) 
= 7

eval' (Add (Add (Val 2) (Val 5)) (Val 10)) 
= 17

eval' (Add (Add (Val 1) (Val 5)) (Add (Val 4) (Val 6))) 
= 16

size (Add (Val 3) (Val 4)) 
= 2

size (Add (Add (Val 2) (Val 5)) (Val 10)) 
= 3

size (Add (Add (Val 1) (Val 5)) (Add (Val 4) (Val 6))) 
= 4

-}

{- 7. 

data Maybe a = Nothing | Just a

instance Eq a => (Maybe a) where
    Nothing  == Nothing  = True
    (Just x) == (Just y) = x == y
    _        == _        = False

instance Eq a => Eq [a] where
    []     == []     = True
    (x:xs) == (y:ys) = (x == y) && (xs == ys)
    _      == _      = False

-}

{- 8. -}

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Iff Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

p6 :: Prop
p6 = Or (Var 'A') (Not (Var 'B'))

p7 :: Prop
p7 = Iff (Var 'A') (Var 'A')

p8 :: Prop
p8 = Imply (And (Var 'A') (Var 'B')) (Or (Var 'A') (Var 'B')) 

-- DeMorgan's Laws:

-- [P ^ (Q V R)] <-> [(P ^ Q) V (P ^ R)] :: Tautology
p9 :: Prop
p9 = Iff (And (Var 'P') (Or (Var 'Q') (Var 'R'))) (Or (And (Var 'P') (Var 'Q')) (And (Var 'P') (Var 'R')))
 
-- [P V (Q ^ R)] <-> [(P V Q) ^ (P V R)] :: Tautology
p10 :: Prop
p10 = Iff (Or (Var 'P') (And (Var 'Q') (Var 'R'))) (And (Or (Var 'P') (Var 'Q')) (Or (Var 'P') (Var 'R')))

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
eval s (Or p q)    = eval s p || eval s q
eval s (Iff p q)   = eval s p == eval s q

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
vars (Or p q) = vars p ++ vars q
vars (Iff p q) = vars p ++ vars q

{- 

vars p1
= "AA"

vars p2
= "ABA"

vars p3
= "AAB"

vars p4
= "AABB"

map (3:) [[1],[20]]
= [[3,1],[3,20]]

-}

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
            where bss = bools (n-1)

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

isTaut p5
True

isTaut p6
False

isTaut p7
True

isTaut p8
True

isTaut p9
True

isTaut p10
True

-}

{- 9. -}


-- Abstract machine

-- data Expr' = Val' Int | Add' Expr' Expr' | Mult' Expr' Expr'

-- value :: Expr' -> Int
-- value (Val' n) = n
-- value (Add' x y) = value x + value y
-- value (Mult' x y) = value x * value y

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Mult x y) = value x * value y

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

data Op = EVALADD Expr | EVALMULT Expr | ADD Int | MULT Int

eval'' :: Expr -> Cont -> Int
eval'' (Val n)    c = exec c n
eval'' (Add x y)  c = eval'' x (EVALADD y : c)
eval'' (Mult x y) c = eval'' x (EVALMULT y : c)

exec :: Cont -> Int -> Int
exec []               n = n
exec (EVALADD  y : c) n = eval'' y (ADD n : c)
exec (EVALMULT y : c) n = eval'' y (MULT n : c)
exec (ADD      n : c) m = exec c (n + m)
exec (MULT     n : c) m = exec c (n * m)


value' :: Expr -> Int
value' e = eval'' e []


{- 

Need to trace through the following when I have more time:

value' (Add (Val 2) (Val 3))
= 5

value' (Mult (Val 2) (Val 3))
= 6

value' (Add (Mult (Val 2) (Val 3)) (Val 4))
= 10


-}





{- Chapter 16.9 Exercises -}

{- 1. 

Base case:

add Zero (Succ m)
= Succ m
= Succ (add Zero m) -- unapplying add

Inductive step:

add (Succ n) (Succ m)
= Succ (add n (Succ m)) 
= Succ (Succ (add n m)) -- induction hypothesis
= Succ (add (Succ n) m) -- unapplying add

-}

{- 2. 

Base case:

add Zero m
= m
= add m Zero (since add n Zero = n)

Inductive step:

add n m
= add (Succ n) m
= Succ (add n m)
= Succ (add m n) -- induction hypothesis
= add m (Succ n) -- property of add

-}

{- 3. 

Definition of replicate:

replicate :: Int -> a -> [a]
replicate 0 = []
replicate n x = x : replicate (n-1) x

Base case:

all (== x) (replicate 0 x)
= all (== x) []
= True

Inductive step:

all (== x) (replicate (n+1) x)
= all (== x) (x : replicate (n) x)
= ((== x) x) && (all (== x) (replicate (n) x)) -- induction hypothesis
= True && True
= True

-}

{- 4. 

Given definitions:

[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

Base case:

[] ++ []
= []

Inductive step:

(x:xs) ++ [] 
= x : (xs ++ [])
= x : xs

Base case:

[] ++ (ys ++ zs)
= ys ++ zs
= ([] ++ ys) ++ zs -- unapplying ++

Inductive step:

(x:xs) ++ (ys ++ zs)
= x : (xs ++ (ys ++ zs))
= x : ((xs ++ ys) ++ zs) -- induction hypothesis 
= (x : (xs ++ ys)) ++ zs -- unapplying ++
= ((x : xs) ++ ys) ++ zs -- unapplying ++

-}

{- 5. 

Base case:

take 0 xs ++ drop 0 xs
= [] ++ xs
= xs

Base case:

take (n+1) [] ++ drop (n+1) []
= [] ++ []
= []

Inductive step:

take (n+1) (x:xs) ++ drop (n+1) (x:xs)
= x : take (n) xs ++ drop (n) xs
= x : (take (n) xs ++ drop (n) xs)
= x : xs -- induction hypothesis

-}

{- 6. -}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

countLeaves :: Tree a -> Int
countLeaves (Leaf _) = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

countNodes :: Tree a -> Int
countNodes (Leaf _) = 0
countNodes (Node l r) = 1 + countNodes l + countNodes r


{-

> countLeaves (Node (Node (Leaf 2) (Leaf 5)) (Leaf 10))
3

> countNodes (Node (Node (Leaf 2) (Leaf 5)) (Leaf 10))
2

Property:

leaves t = nodes t + 1

Base case:

nodes (Leaf x) + 1
= nodes 0 + 1
= 1
= leaves (Leaf x) 

Inductive step:

nodes (Node l r) + 1 
= (1 + nodes l + nodes r) + 1
= (1 + nodes l + nodes r) + 1
= (1 + nodes l) + (1 + nodes r)
= leaves l + leaves r
= leaves (Node l r) -- unapplying leaves

-}

{- 7. 

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just (g x)


fmap id (Just x)
= Just (id x) -- preserves the identity function (fmap id = id)
= Just x

fmap (f . g) (Just x)
= Just ((f . g) x)
= Just (f (g x))
= fmap f (Just (g x))
= fmap f . fmap g (Just x) -- preserves function composition (fmap (g . h) = fmap g . fmap h)

-}

{- 8. -}

-- To be implemented later.

{- 9. -}

-- To be implemented later.

{- 10. -}

-- To be implemented later.

{- 11. -}

data Expr = Val Int | Add Expr Expr deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]

data Op = PUSH Int | ADD deriving Show

exec :: Code -> Stack -> Stack
exec []           s        = s
exec (PUSH n : c) s        = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n+m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

e = Add (Add (Val 2) (Val 3)) (Val 4)

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))


{-

Property:

comp' e c = comp e ++ c

Base case:

comp' Val n c
= comp' (Val n) ++ c -- applying comp' property 
= [PUSH n] ++ c      -- applying comp
= PUSH n : c

Inductive step:

comp' (Add x y) c
= comp (Add x y) ++ c
= (comp x ++ comp y ++ [ADD]) ++ c
= comp x ++ (comp y ++ [ADD] ++ c) -- ++ is associative
= comp' x (comp y ++ [ADD] ++ c) -- induction hypothesis for x
= comp' x (comp y ++ ([ADD] ++ c))
= comp' x (comp' y ([ADD] ++ c)) -- induction hypothesis for y
= comp' x (comp' y (ADD : c))

-}













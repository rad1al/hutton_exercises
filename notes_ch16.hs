{- Chapter 16 :: Reasoning about programs -}

double :: Int -> Int
double x = x + x

isZero :: Int -> Bool
isZero 0 = True
isZero n | n /= 0 = False

not' :: Bool -> Bool
not' False = True
not' True = False

data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero m     = m
add (Succ n) m = Succ (add n m)

{- 

> add Zero (Succ Zero)
Succ Zero

> add (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
Succ (Succ (Succ (Succ (Succ Zero))))

replicate :: Int -> a -> [a]
replicate 0 = []
replicate n x = x : replicate (n-1) x

------------------ Induction on lists -----------------

reverse (reverse [])
= reverse []
= []

reverse (reverse (x:xs))
= reverse (reverse xs ++ [x]))
= reverse [x] ++ reverse (reverse xs) -- distributive property
= [x] ++ xs
= x : xs

Base case:
reverse ([] ++ ys)
= reverse ys
= reverse ys ++ []
= reverse ys ++ reverse []

Inductive case:
reverse ((x:xs) ++ ys)
= reverse (x : (xs ++ ys))
= reverse (xs ++ ys) ++ [x]
= (reverse ys ++ reverse xs) ++ [x]
= reverse ys ++ (reverse xs ++ [x]) -- unapplying second reverse
= reverse ys ++ reverse (x:xs)

-}

reverse'' :: [a] -> [a] -> [a]
reverse'' [] ys = ys
reverse'' (x:xs) ys = reverse'' xs (x:ys)

{-

> reverse'' [1,2,3] [4,5,6]
[3,2,1,4,5,6]

-}

reverse''' :: [a] -> [a]
reverse''' xs = reverse'' xs []


{-

> reverse''' [1,2,3]
[3,2,1]


reverse''' [1,2,3]
= reverse'' [1,2,3] []
= reverse'' [2,3] (1:[])
= reverse'' [3] (2:1:[])
= reverse'' [] (3:2:1:[])
= 3:2:1:[]

-}


data Tree = Leaf Int | Node Tree Tree deriving Show

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r


flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns = n : ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

flatten'' :: Tree -> [Int]
flatten'' t = flatten' t []

{-

> flatten (Node (Leaf 1) (Leaf 2))
[1,2]

> flatten' (Leaf 1) []
[1]

> flatten'' (Node (Leaf 1) (Leaf 2))
[1,2]


-}


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

{-

> eval e
9

> comp e
[PUSH 2,PUSH 3,ADD,PUSH 4,ADD]

> exec (comp e) []
[9]


-}

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))


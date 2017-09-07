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


















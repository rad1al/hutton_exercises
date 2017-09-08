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

{- 7. -}

-- To be implemented later.

{- 8. -}

-- To be implemented later.

{- 9. -}

-- To be implemented later.

{- 10. -}

-- To be implemented later.

{- 11. -}

-- To be implemented later.

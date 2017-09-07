{- Chapter 15.9 Exercises -}


{- 1. 

1 + (2*3) -- 2*3 is both

(1+2) * (2+3) -- 1+2 and 2+3 are redexes with the first being innermost

fst (1+2, 2+3) -- 1+2, 2+3 and fst (1+2, 2+3) with the first being innermost and last outermost 

(\x -> 1 + x) (2*3) -- 2*3 and (\x -> 1 + x) (2*3) with the first being innermost and last outermost

-}

{- 2. 

fst (1+2,2+3)
= 1+2
= 3

Outermost evaluation avoids evaluation of the second argument which takes fewer reduction steps.

-}

{- 3. 

mult 3 4
= \x -> (\y -> x * y) 3 4
= (\y -> 3 * y) 4
= 3 * 4
= 12

-}

{- 4. -}

fibs :: [Integer]
fibs = 0 : 1 : map (\(x,y) -> x + y) (zip (fibs) (tail fibs)) -- According to chapter 16, appending (++) is inefficient.

{-

> take 10 fibs
[0,1,1,2,3,5,8,13,21,34]

-}


{- 5. -}

data Tree a = Leaf | Node (Tree a) a (Tree a) 
              deriving Show


repeatTree :: a -> Tree a
repeatTree x = Node t x t
               where t = repeatTree x

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _ = Leaf
takeTree _ Leaf = Leaf
takeTree n (Node l x r) = Node (takeTree (n-1) l) x (takeTree (n-1) r)

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree

{-

> takeTree 2 (Node (Node (Leaf) 2 (Leaf)) 5 (Node (Leaf) 3 (Leaf)))
Node (Node Leaf 2 Leaf) 5 (Node Leaf 3 Leaf)

> takeTree 1 (Node (Node (Leaf) 2 (Leaf)) 5 (Node (Leaf) 3 (Leaf)))
Node Leaf 5 Leaf

> takeTree 0 (Node (Node (Leaf) 2 (Leaf)) 5 (Node (Leaf) 3 (Leaf)))
Leaf

> replicateTree 0 10
Leaf

> replicateTree 1 10
Node Leaf 10 Leaf

> replicateTree 2 10
Node (Node Leaf 10 Leaf) 10 (Node Leaf 10 Leaf)

> replicateTree 3 10
Node (Node (Node Leaf 10 Leaf) 10 (Node Leaf 10 Leaf)) 10 (Node (Node Leaf 10 Leaf) 10 (Node Leaf 10 Leaf))

-}

{- 6. -}

next :: Double -> Double -> Double
next n a = (a + n/a) / 2

sqroot :: Double -> Double
sqroot n = within 0.00001 $ iterate (next n) 1

within :: Double -> [Double] -> Double
within v (x:y:xs) | abs (x - y) < v = x
                  | otherwise = within v (y:xs) 

{-

> sqroot 9
3.000000001396984

> sqroot 2
1.4142156862745097

> sqroot 0
1.52587890625e-5

-}




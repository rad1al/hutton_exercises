{- Chapter 9 :: The countdown problem -}

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- valid function to decide if the application of 
-- an operation to two positive natural numbers
-- gives another positive natural number.

{-

-- valid basic definition:

valid :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Sub x y = x > y
valid Mul x y = True
valid Div x y = x `mod` y == 0

-}

-- valid defintion which exploits algebraic properties:
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                          brak (Val n) = show n
                          brak e       = "(" ++ show e ++ ")"

{-

show (Val 1)
= "1"

show (App Add (Val 1) (Val 10))
= "1+10"

show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
= "1+(2*3)"

-}

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, 
                                  y <- eval r,
                                valid o x y]


{-

values (App Add (Val 1) (App Mul (Val 2) (Val 3)))
= [1,2,3]

eval (App Add (Val 2) (Val 3))
= [5]

eval (App Sub (Val 2) (Val 3))
= []

eval (App Add (Val 1) (App Mul (Val 2) (Val 3)))
= [7]
-}

-- powerset

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where
                yss = subs xs

{-

subs [3]
= subs [] ++ map (3:) subs []
= [[]] ++ map (3:) [[]]
= [[]] ++ [[3]]
= [[],[3]]

subs [2,3]
= subs [3] ++ map (2:) subs [3]
= [[],[3]] ++ map (2:) [[],[3]]
= [[],[3]] ++ [[2],[2,3]]
= [[],[3],[2],[2,3]]

subs [1,2,3]
= subs [2,3] ++ map (1:) (subs [2,3])
= [[],[3],[2],[2,3]] ++ map (1:) [[],[3],[2],[2,3]]
= [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-}

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

{-

interleave 1 [4]
= (1:4:[]) : map (4:) interleave 1 []
= (1:4:[]) : map (4:) [[1]]
= (1:4:[]) : [[4,1]]
= [1,4] : [[4,1]]
= [[1,4],[4,1]]

interleave 1 [3,4]
= (1:3:[4]) : map (3:) interleave 1 [4]
= (1:3:[4]) : map (3:) [[1,4],[4,1]]
= (1:3:[4]) : [[3,1,4],[3,4,1]]
= [[1,3,4],[3,1,4],[3,4,1]]

interleave 1 [2,3,4]
= (1:2:[3,4]) : map (2:) (interleave 1 ys)  
= [1,2,3,4] : map (2:) interleave 1 [3,4]  
= [1,2,3,4] : map (2:) [[1,3,4],[3,1,4],[3,4,1]]  
= [1,2,3,4] : [[2,1,3,4],[2,3,1,4],[2,3,4,1]]  
= [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]  

-}

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

{-

perms [2,3]
= concat (map (interleave 2) (perms [3]))
= concat (map (interleave 2) [[3]])
= concat [[[2,3],[3,2]]]
= [[2,3],[3,2]]

perms [1,2,3]
= concat (map (interleave 1) (perms [2,3]))
= concat (map (interleave 1) [[2,3],[3,2]])
= concat [[[1,2,3],[2,1,3],[2,3,1]],[[1,3,2],[3,1,2],[3,2,1]]]
= [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-}

-- get all permutations of the powerset subsequences

choices :: [a] -> [[a]]
choices = concat . map perms . subs

{-

choices [1,2,3]
= [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]


elem 5 [1,2,3,5]
= True

eval (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))
= [765]

-- Reminder: eval returns either singleton or empty list.

-}

e1 :: Expr
e1 = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
    elem (values e) (choices ns) && eval e == [n]

{-

solution e1 [1,3,7,10,25,50] 765
= True

-}

-- brute force solution

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

{-

split [1,2,3,4]
= [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

-}


exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l       <- exprs ls,
                r       <- exprs rs,
                e       <- combine l r]

{-

exprs [1,2]
= [1+2,1-2,1*2,1/2]

exprs [1,2,3]
= [1+(2+3),1-(2+3),1*(2+3),1/(2+3),1+(2-3),1-(2-3),1*(2-3),1/(2-3),
1+(2*3),1-(2*3),1*(2*3),1/(2*3),1+(2/3),1-(2/3),1*(2/3),1/(2/3),
(1+2)+3,(1+2)-3,(1+2)*3,(1+2)/3,(1-2)+3,(1-2)-3,(1-2)*3,(1-2)/3,
(1*2)+3,(1*2)-3,(1*2)*3,(1*2)/3,(1/2)+3,(1/2)-3,(1/2)*3,(1/2)/3]

-}

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, 
                      e   <- exprs ns',
                      eval e == [n]]

{- 

check countdown.hs for performance test 

-}

-- combining generation and evalution

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                    lx       <- results ls,
                    ry       <- results rs,
                    res      <- combine' lx ry]


combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops,
                                                   valid o x y]



solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                       (e,m) <- results ns',
                       m == n]



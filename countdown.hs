{- Countdown Problem performance test 

To compile:
$ ghc -O2 countdown.hs

To run;
$ ./countdown

-- Timing --

$ time ./countdown 

(using brute force)

real	0m5.131s
user	0m5.054s
sys		0m0.067s

************

(combining generation and evaluation)

real	0m0.485s
user	0m0.470s
sys		0m0.009s

************

(exploiting algebraic properties)

real	0m0.058s
user	0m0.051s
sys		0m0.004s

-}

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

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


values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, 
                                  y <- eval r,
                                valid o x y]



subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where
                yss = subs xs


interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))


choices :: [a] -> [[a]]
choices = concat . map perms . subs


e1 :: Expr
e1 = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
    elem (values e) (choices ns) && eval e == [n]



split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- Variation: split [] = [([],[])]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l       <- exprs ls,
                r       <- exprs rs,
                e       <- combine l r]


combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, 
                      e   <- exprs ns',
                      eval e == [n]]

-- main :: IO ()
-- main = print (solutions [1,3,7,10,25,50] 765)

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

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)




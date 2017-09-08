import Data.List (sortBy)
import Data.Ord (comparing)

{- Chapter 9.11 Exercises -}

{- 1. -}

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

-- choices :: [a] -> [[a]]
-- choices = concat . map perms . subs

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

{-

choices [1,2,3]
= [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-}

{- 2. -}

takeOut :: Eq a => a -> [a] -> [a]
takeOut v [] = []
takeOut v (x:xs) | v == x    = xs
                 | otherwise = x : takeOut v xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []     ys = True 
isChoice (x:xs) [] = False 
isChoice (x:xs) ys = elem x ys && isChoice xs (takeOut x ys)

{- 

takeOut 5 [10,5,20]
[10,20]

isChoice [1,2] [1,2,3]
= elem 1 [1,2,3] && isChoice [2] [2,3]
= True && elem 2 [2,3] && isChoice [] [3]
= True && True && True
= True

isChoice [1,2,3] [1,2]
= elem 1 [1,2] && isChoice [2,3] [2]
= True && elem 2 [2] && isChoice [3] []
= True && True && False
= False

-}

{- 3. 

It would lead to non-termination because recursive calls to exprs
would no longer be guaranteed to reduce the length the list.

-}

{- 4. -}

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"


valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && x `mod` y == 0 
valid Exp x y = x /= 1 && y > 1
-- had to change 'y /= 1' to 'y /= 0' to deal with the division by zero error.

{-
valid :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Sub x y = x > y
valid Mul x y = True
valid Div x y = x `mod` y == 0
-}

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

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

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l       <- exprs ls,
                r       <- exprs rs,
                e       <- combine l r]

allExpressions :: [Int] -> [Expr]
allExpressions ns = [e | ns' <- choices ns, e <- exprs ns']

possibleExpressions :: [Int] -> Int
possibleExpressions = length . allExpressions

successfulExpressions :: [Int] -> Int
successfulExpressions = length . filter (not . null) . map eval . allExpressions

{-

map eval $ allExpressions [1,2]
= [[2],[1],[3],[],[],[],[],[1],[],[]]

Takes a while to run: 

possibleExpressions [1,3,7,10,25,50]
= 33665406

successfulExpressions [1,3,7,10,25,50]
= 4672540

-}

{- 5. 

-- valid definition with arbitrary integers
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0

Using the above valid function definition, we obtain: 

successfulExpressions [1,3,7,10,25,50]
= 10839369

-}

{- 6. -}

{- a. -} 


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


solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, 
                      e   <- exprs ns',
                      eval e == [n]]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                       (e,m) <- results ns',
                       m == n]

{- 

(solutions [1,3,7,10,25,50] 765)
divide by zero?

-}

{- b. -}

calcExprDiff :: [Int] -> Int -> [(Expr, Int)]
calcExprDiff ns n = [(e, abs (m-n)) | ns' <- choices ns, (e,m) <- results ns']

{-

calcExprDiff [1,2,50] 90
= [(50,40),(2,88),(2+50,38),(2*50,10),(2^50,1125899906842534),
(50-2,42),(50/2,65),(50^2,2410),(1,89),(1+50,39),(50-1,41),
(1+2,87),(2-1,89),(1+(2+50),37),(1+(2*50),11),(1+(2^50),1125899906842535),
((1+2)+50,37),((1+2)*50,60),((1+2)^50,6048575297968530287),(2+(1+50),37),
(2*(1+50),12),(2^(1+50),2251799813685158),((2-1)+50,39),(2+(50-1),39),
(2*(50-1),8),(2^(50-1),562949953421222),((2+50)-1,39),((2*50)-1,9),((2^50)-1,1125899906842533),
(1+(50-2),41),(1+(50/2),64),(1+(50^2),2411),((1+50)-2,41),((1+50)^2,2511),(50-(1+2),43),
(50^(1+2),124910),((50-1)-2,43),((50-1)^2,2311),(50-(2-1),41),
((50-2)-1,43),((50/2)-1,66),((50^2)-1,2409)]


sortBy (comparing snd) [(20, 20), (30,15)]
= [(30,15),(20,20)]


-}


-- Take the 10 solutions with the lowest difference.
lowestDiffSolutions :: [(Expr,Int)] -> [(Expr,Int)]
lowestDiffSolutions = take 10 . sortBy (comparing snd)

{- 

lowestDiffSolutions $ calcExprDiff [1,2,50] 90
= [(2*(50-1),8),((2*50)-1,9),(2*50,10),(1+(2*50),11),(2*(1+50),12)]

lowestDiffSolutions $ calcExprDiff [1,3,7,10,25,50] 765
= [(3*((7*(50-10))-25),0),((25-10)*(1+50),0),((25-(3+7))*(1+50),0),(((25-3)-7)*(1+50),0),(((25-7)-3)*(1+50),0)]

-}

{- c. -}

exprCmplxty :: Expr -> Int
exprCmplxty (Val n) = 0
exprCmplxty (App Add l r) = 1 + exprCmplxty l + exprCmplxty r
exprCmplxty (App Sub l r) = 1 + exprCmplxty l + exprCmplxty r
exprCmplxty (App Mul l r) = 10 + exprCmplxty l + exprCmplxty r
exprCmplxty (App Div l r) = 10 + exprCmplxty l + exprCmplxty r
exprCmplxty (App Exp l r) = 100 + exprCmplxty l + exprCmplxty r

orderSolnsByCmplxty :: [Int] -> Int -> [Expr]
orderSolnsByCmplxty ns n = (map fst) $
                           sortBy (comparing snd) 
                           $ map (\(x,y) -> (x, exprCmplxty x)) (lowestDiffSolutions (calcExprDiff ns n))

{-

orderSolnsByCmplxty [1,2,50] 90
= [1+(2+50),(1+2)+50,2*50,
   2*(50-1),(2*50)-1,1+(2*50),
   2*(1+50),(2/1)*50,2*(50/1),
   (2*50)/1]

orderSolnsByCmplxty [1,3,7,10,25,50] 765
= [(25-10)*(1+50),(25-(3+7))*(1+50),
   ((25-3)-7)*(1+50),((25-7)-3)*(1+50),
   ((3-1)+7)*(10+(25+50)),((3-1)+7)*((10+25)+50),
   (3+(7-1))*(10+(25+50)),(3+(7-1))*((10+25)+50),
   ((3+7)-1)*(10+(25+50)),3*((7*(50-10))-25)]

-}




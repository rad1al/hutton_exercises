{- Chapter 17 :: Calculating compilers -}

data Expr = Val Int | Add Expr Expr deriving Show

{-

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

> eval (Add (Val 1) (Val 2))
3

Property:

eval' e s = eval e : s

-}

type Stack = [Int]

eval' :: Expr -> Stack -> Stack
eval' (Val n)   s = push n s
eval' (Add x y) s = add (eval' y (eval' x s))

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : s) = n+m : s

eval :: Expr -> Int
eval e = head (eval' e [])

{-

> eval (Add (Val 1) (Val 2))
3

eval (Add (Val 1) (Val 2))
= head (eval' (Add (Val 1) (Val 2)) [])
= head (add (eval' (Val 2) (eval' (Val 1) [])))
= head (add (eval' (Val 2) (push 1 [])))
= head (add (push 2 (push 1 [])))
= head (add (2 : 1 : []))
= head (3 : [])
= 3

------------------ continuation-passing style -----------------

we seek to define a function eval'':

eval'' :: Expr -> Cont -> Cont
eval'' e c s = c (eval' e s)

-}

type Cont = Stack -> Stack

eval'' :: Expr -> Cont -> Cont
eval'' (Val n)   c s = c (push n s)
eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

eval''' :: Expr -> Cont
eval''' e s = eval'' e id s

{-

> eval''' (Add (Val 1) (Val 2)) []
[3]

eval''' (Add (Val 1) (Val 2)) []
= eval'' (Add (Val 1) (Val 2)) id []
= eval'' (Val 1) (eval'' (Val 2) (id . add)) []
= eval'' ((Val 2) (id . add)) (push 1 []) -- applying outer eval''
= eval'' (Val 2) (id . add) (push 1 [])
= (id . add) (push 2 (push 1 []))
= id . (add (push 2 (push 1 [])))
= id . (add (2 : 1 : []))
= id . [3]
= [3]

------------------ Defunctionalising -----------------

-}

haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . push n

addC :: Cont -> Cont
addC c = c . add

eval'''' :: Expr -> Cont
eval'''' e = eval''''' e haltC

eval''''' :: Expr -> Cont -> Cont
eval''''' (Val n)   c = pushC n c
eval''''' (Add x y) c = eval''''' x (eval''''' y (addC c))


{-

> eval'''' (Add (Val 1) (Val 2)) []
[3]

-}

data Code = HALT | PUSH Int Code | ADD Code
            deriving Show

{-

HALT :: Code
PUSH :: Int -> Code -> Code
ADD :: Code -> Code

exec :: Code -> Cont
exec HALT = haltC
exec (PUSH n c) = pushC n (exec c)
exec (ADD c) = addC (exec c)

> exec HALT [1]
[1]

> exec (PUSH 2 (HALT)) [1]
[2,1]

-}

exec :: Code -> Stack -> Stack
exec HALT       s              = s
exec (PUSH n c) s              = exec c (n : s)
exec (ADD c)    (m : n : s)    = exec c (n+m : s)

comp :: Expr -> Code -- function which compiles an expression to code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n)   c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))


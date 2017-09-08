{- Chapter 17.8 Exercises -}

{- 1. -}

-- To be implemented later.

data Expr = Val Int 
          | Add Expr Expr
          | Throw
          | Catch Expr Expr 
            deriving Show

type Stack = [Int]

data Code = HALT | PUSH Int Code | ADD Code
            deriving Show

exec :: Code -> Stack -> Stack
exec HALT       s              = s
exec (PUSH n c) s              = exec c (n : s)
exec (ADD c)    (m : n : s)    = exec c (n+m : s)

comp :: Expr -> Code -- function which compiles an expression to code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n)   c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = case eval x of
                      Just n -> case eval y of
                           Just m -> Just (n + m)
                           Nothing -> Nothing
                      Nothing -> Nothing
eval Throw = Nothing
eval (Catch x h) = case eval x of
                        Just n -> Just n
                        Nothing -> eval h
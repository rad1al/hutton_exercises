{- Chapter 12.5 Exercises -}

{- 1. -}


data Tree a = Leaf a | Node (Tree a) a (Tree a)
              deriving Show

{-

An example of the Tree a = Leaf a | Node (Tree a) a (Tree a):

> Node (Leaf 1) 2 (Leaf 3)
Node (Leaf 1) 2 (Leaf 3)

-}


instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)


{- 2. 

> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

instance Functor ((->) a) where
    -- fmap :: (x -> y) -> a x -> a y
    -- fmap :: (x -> y) -> ((-> a) x) -> ((-> a) y)
    -- fmap :: (x -> y) -> (a -> x) -> (a -> y)
    fmap = (.)

-}


{- 3. 

> :t const
const :: a -> b -> a

instance Applicative ((->) a) where
    -- pure :: a -> f a
    -- pure :: a -> ((->) b a)
    -- pure :: a -> (b -> a)
    pure = const

    -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
    g <*> h = \x -> g x (h x)

-}


{- 4. 

> :t repeat
repeat :: a -> [a]

> repeat 5
[5,5..]


-}

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)

    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]


{- 5. 



Work out the types for the variables in the four applicative laws.

Not sure if these are correct:

pure id <*> x 
> (f a, Applicative f) => f a

pure (g x)
> (a, Applicative f) => f a

x <*> pure y
> (f (a -> b), a, Applicative f) => f b

x <*> (y <*> z)
> (f (a -> b), f(b -> c), (f a), Applicative f) => f c


Reminder:

Applicative laws:

Applicative functors are required to satisfy four 
equational laws:

pure id <*> x   = x
pure (g x)      = pure g <*> pure x
x <*> pure y    = pure (\g -> g y) <*> x
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

-}


{- 6. 

instance Monad (a (->)) gives error when trying to compile:
Illegal instance declaration for ‘Monad (a (->))’

Not sure if correct:

instance Monad ((->) a) where
    -- (>>=) :: m x -> (x -> m y) -> m y
    -- (>==) :: ((->) a x) -> (x -> ((->) a y)) -> ((->) a y)
    -- (>==) :: (a -> x) -> (x -> a -> y) -> (a -> y)
    >>= f g = \a -> g (f a) a 

-}




{- 7. -}

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
    deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x)   = Var (f x)
    fmap _ (Val x)   = Val x
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    (<*>) (Var f) x = fmap f x
    (<*>) _ (Val x) = Val x

instance Monad Expr where
    -- (>>=) :: Expr a -> (a -> Expr b) -> b
    (>>=) (Var x) f = f x
    (>>=) (Val x) _ = Val x

{- 8. -}

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    -- fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))
    -- fmap g st = st >>= return . g
    fmap g st = do s <- st
                   return $ g s

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s)) 

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    -- stf <*> stx = S (\s -> 
    --                 let (f,s') = app stf s
    --                     (x,s'') = app stx s' in (f x, s''))
    stf <*> stx = do f <- stf
                     x <- stx
                     return $ f x

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b 
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

    -- return :: a -> ST a
    -- return x = S (\s -> (x,s))


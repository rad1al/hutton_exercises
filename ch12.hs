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
-- To be implemented.

{- 8. -}
-- To be implemented.
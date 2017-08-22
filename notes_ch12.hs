{- Chapter 12 :: Monads and more -}

import Data.Char (isDigit, digitToInt)

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

{-

inc [20,30,2]
= [21,31,3]

-}

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

{-

sqr [1,5,10]
= [1,25,100]

-}

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

{-

map' (+1) [10,20,30]
= [11,21,31]

-}

inc' = map (+1)
sqr' = map (^2)

{-

Built into GHC.Base/Prelude:

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    -- fmap :: (a -> a) -> [a] -> [b]
    fmap = map

data Maybe a = Nothing | Just a

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just (g x)

-}


{-

fmap (+1) Nothing
= Nothing

fmap (*2) (Just 3)
= Just 6

fmap not (Just False)
= Just True


-}

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)


{-

> fmap length (Leaf "abc")
Leaf 3

> fmap even (Node (Leaf 1) (Leaf 2))
Node (Leaf False) (Leaf True)

-}

{-

instance Functor IO where
    -- fmap :: (a -> b) -> IO a -> IO b
    fmap g mx = do {x <- mx; return (g x)}

-}

{-

> fmap show (return True)
"True"

> fmap show (return False)
"False"

Benefits of using functors:

1. The function fmap can be used to process the elements of any 
structure tha tis functorial - that is, we can use the same name 
for fucntions that are essentially the same, rather than having 
to invent a separate name for each instance.

2. We can define generic functions thatcan be used with any functor. 
For example, the earlier function that increments each integer in a 
list can be generalized to any functorial type by simply using fmap 
rather than map.

-}

inc'' :: Functor f => f Int -> f Int
inc'' = fmap (+1)

{-

inc'' (Just 1)
= Just 2

inc'' [1,2,3,4,5,6]
= [2,3,4,5,6,7]

inc'' (Node (Leaf 1) (Leaf 2))
= Node (Leaf 2) (Leaf 3)



-}

data Lst a = T [a] -- declare new list type
                deriving Show

{-

fmap0 :: a -> f a

fmap1 :: (a -> b) -> f a -> f b

fmap2 :: (a -> b -> c) -> f a -> f b -> f c

fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

-------------------------------------------------------

pure :: a -> f a

(<*>) :: f (a -> b) -> f a -> f b

> :t pure
pure :: Applicative f => a -> f a

> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

-------------------------------------------------------

fmap0 :: a -> f a
fmap0 = pure

fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y

fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g x y z = pure g <*> x <*> y <*> z

-}

{-

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure = Just

    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    (Just g) <*> mx = fmap g mx

> pure (+1) <*> Just 1
Just 2

> pure (+) <*> Just 1 <*> Just 2
Just 3

> pure (+) <*> Nothing <*> Just 2
Nothing

-------------------------------------------------------

Applicatives or "applicative functors" are the class of 
functors that support pure and <*> functions.

The standard prelude contains the following instance 
declaration:

instance Applicative [] where
    -- pure :: a -> [a]
    pure x = [x]

    -- (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]

> pure (+1) <*> [1,2,3]
[2,3,4]

= [(+1)] <*> [1,2,3]
= [(+1) 1, (+1) 2, (+1) 3]
= [2, 3, 4]

> pure (+) <*> [1] <*> [2]
[3]

= ([(+)] <*> [1]) <*> [2]
= ([(+) 1]) <*> [2]
= [((+) 1) 2]

> pure (*) <*> [1,2] <*> [3,4]
[3,4,6,8]

= [(*)] <*> [1,2] <*> [3,4]
= [(*) 1, (*) 2] <*> [3,4]
= [(*) 1 3, (*) 1 4, (*) 2 3, (*) 2 4]

Using ghci to check: 
> [(*) 1, (*) 2] <*> [3,4]
[3,4,6,8]

-}

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x*y | x <- xs, y <- ys]

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys

{-

prods' [1,2] [10,20,30]
= [10,20,30,20,40,60]

instance Applicative IO where
    -- pure :: a -> IO a
    pure = return

    -- (<*>) :: IO (a -> b) -> IO a -> IO b
    mg <*> mx = do {g <- mg; x <- mx; return (g x)} 



-}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

{-

> getChars 3 -- Typing in 'abc' gives:
"abc"

> getChars 2 -- Typing in 'xyz' gives:
"xy"

The standard library provides the following function:

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs

-}

getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

{-

getChars' 3 -- Typing in 'abc' gives:
"abc"

> :t replicate 3 getChar
replicate 3 getChar :: [IO Char]


-------------------------------------------------------

Applicative laws:

Applicative functors are required to satisfy four 
equational laws:

pure id <*> x   = x
pure (g x)      = pure g <*> pure x
x <*> pure y    = pure (\g -> g y) <*> x
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z


-}

{------------------ Monads -------------------------

From fubo on Reddit (https://www.reddit.com/r/explainlikeimfive/comments/kzw5j/eli5_monads_programming/c2omloo/):

Monad is not a data type, like a floating-point number, or a list of records. Monad is a set of 
algebraic properties that a data type can have. Saying that a data type "is a monad" (or, more 
technically, "forms a monad") means saying that a data type follows a certain set of mathematical 
equalities for some operations on that data type.

Okay, what is an "algebraic property"? Well, here is an example: commutativity. We say that 
multiplication of integers is "commutative" because for any integers X and Y, X·Y = Y·X. No matter 
what the integers are, this equality is true. Associativity is another algebraic property, which means 
that changing the grouping of operations doesn't change their effects: (A+B)+C is the same as A+(B+C).

Commutativity and associativity are algebraic properties. As you can see from these examples, an 
algebraic property depends on a type (such as integers) and an operation (such as adding or multiplying). 
We say that the integers "commute over multiplication" (and addition, etc.) — but they do not commute over 
subtraction: X–Y is not generally the same as Y–X.

Some algebraic properties depend on more than one operation, for instance distributivity, which says that 
A·(B+C) = A·B + A·C. We say that multiplication on the integers distributes over addition.

Also, when a type has operations with various particular sets of algebraic properties, we sometimes call that 
type by a special name, such as ring or group. Monad is a different set of algebraic properties. It means there 
are certain operations, called bind and join and return, that work in a particular way. These operations are a 
little more abstract than things like addition and multiplication. Monad doesn't say what these operations 
actually do; it just says that they have to relate to each other in a particular fashion. I won't go into exactly 
what these are, but basically return is about making new monadic values from non-monadic ones; join is about 
collapsing nested groupings (kind of like associativity); and bind is about funneling values through functions. 
There are certain equations that these operations have to meet; and if they do, then we can say that the type 
is (or forms) a monad.

It happens that a whole bunch of useful things turn out to fit the monad description. One of these is 
"I/O operations" — which is a programmer way of saying, "those events in a computer program that interact with 
an outside world, like your screen and keyboard, or the Internet". But there are lots of other monads. Lists 
form a monad. Functions that return a result while reading from a stream of data, are a monad. Functions that 
update a shared global state variable are a monad. And so on.

...

From functions on monads, it's possible to define imperative-style programming within pure-functional programming. 
That's what the "do notation" in Haskell is.

However, more generally, monads work with lazy evaluation to make it possible to fully describe a computation 
separately from when you actually want it computed. For an example, take a look at Parsec, a monadic parser library, 
which makes writing a parser almost as easy as writing down the grammar — and what you write is not merely a 
specialized parser description language; it's actual code that can use the full power of Haskell.


-}

data Expr = Val Int | Div Expr Expr -- deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

{-

> eval (Div (Val 10) (Val 2))
5

> eval (Div (Val 10) (Val 0))
*** Exception: divide by zero

-}

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)


eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of 
                  Nothing -> Nothing
                  Just n -> case eval' y of
                     Nothing -> Nothing
                     Just m -> safediv n m

{-

> eval' (Div (Val 1) (Val 0))
Nothing

> eval' (Div (Div (Val 10) (Val 2)) (Val 5))
Just 1

> eval' (Div (Div (Val 0) (Val 2)) (Val 5))
Just 0

> eval' (Div (Div (Val 2) (Val 0)) (Val 5))
Nothing

-}


{-
The following function is not type correct:

eval'' :: Expr -> Maybe Int
eval'' (Val n)   = pure n
eval'' (Div x y) = pure safediv <*> eval'' x <*> eval'' y

ghci states: Couldn't match type ‘Maybe Int’ with ‘Int’

-------------------------------------------------------

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
           Nothing -> Nothing
           Just x -> f x


-}

eval'' :: Expr -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = eval'' x >>= \n -> eval'' y >>= \m -> safediv n m 
-- eval'' (Div x y) = eval'' x >>= (\n -> eval'' y >>= (\m -> safediv n m)) 

{-

> eval'' (Div (Val 1) (Val 0))
Nothing

> eval'' (Div (Div (Val 10) (Val 2)) (Val 5))
Just 1

-------------------------------------------------------

m1 >> \x1 -> 
m2 >> \x2 -> 
.
.
.
mn >> \xn -> 
f x1 x2 ... xn

is basically the same as the notation for interactive 
programming:

do x1 <- m1
   x2 <- m2
   .
   .
   .
   xn <- mn
   f x1 x2 ... xn


-}

eval''' :: Expr -> Maybe Int
eval''' (Val n)   = Just n
eval''' (Div x y) = do n <- eval''' x
                       m <- eval''' y
                       safediv n m

{-

> eval''' (Div (Val 1) (Val 0))
Nothing

> eval''' (Div (Div (Val 10) (Val 2)) (Val 5))
Just 1

-------------------------------------------------------

class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

    return = pure

In the future, return may be removed from the Monad 
class and become a library function with the following
definition:

return :: Applicative f => a -> f a
return = pure

-------------------------------------------------------

instance Monad Maybe where
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing  >>= _ = Nothing
    (Just x) >>= f = f x


instance Monad [] where
    -- (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = [y | x <- xs, y <- f x]

-}

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)

{-

> pairs [1,2] [3,4]
[(1,3),(1,4),(2,3),(2,4)]

Note similarity to a definition using the comprehension
notation:

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x <- xs, y <- ys]

-------------------------------------------------------

instance Monad IO where
    -- return :: a -> IO a
    return x = ... -- return and >>= are built-into the
                   -- the language.

    -- (>>=) :: IO a -> (a -> IO b) -> IO b
    mx >>= f = ...

-}

type State = Int

-- type ST = State -> State

-- type ST a = State -> (a, State)

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s)) 

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s -> 
                     let (f,s') = app stf s
                         (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

-- data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')


rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                        (l',n') = rlabel l n
                        (r',n'') = rlabel r n'

{-

> fst $ rlabel tree 0
Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

> rlabel tree 0
(Node (Node (Leaf 0) (Leaf 1)) (Leaf 2),3)




-}

fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r


{-

> fst $ app (alabel tree) 0
Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)


-}

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

{-

> fst $ app (mlabel tree) 0
Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)


-}


{------------------ Generic Functions ---------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) = do y <- f x
                   ys <- mapM f xs
                   return (y:ys)

-}

-- convert a digit character to its numeric value
conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

{-

> mapM conv "1234"
Just [1,2,3,4]

> mapM conv "123a"
Nothing

> map conv "1234"
[Just 1,Just 2,Just 3,Just 4]


> map conv "123a"
[Just 1,Just 2,Just 3,Nothing]


> :t mapM conv "123a"
mapM conv "123a" :: Maybe [Int]

> :t map conv "123a"
map conv "123a" :: [Maybe Int]
-}

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p [] = return []
filterM p (x:xs) = do b <- p x
                      ys <- filterM p xs
                      return (if b then x:ys else ys)


{-

-- Obtaining the powerset of a list with filterM:
> filterM (\x -> [True, False]) [1,2,3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

-}

-- concat generalised to an arbitrary monad:

join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x <- mx
              return x 


{-

> join [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]

> join [[1,2]]
[1,2]

> join (Just (Just 1))
Just 1

> join (Just Nothing)
Nothing

> join Nothing
Nothing

------------------ Monad laws ------------------

return x >>= f    = f x
mx >>= return     = mx
(mx >>= f) >>= g  = mx >>= (\x -> (f x >>= g))



-}


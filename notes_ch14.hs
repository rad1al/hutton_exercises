{- Chapter 14 :: Monads and more -}

import Data.Monoid -- importing this will cause an "Ambiguous 
                      -- occurrence" error with our user defined ‘Sum’ type 

import Data.Foldable

{-

class Monoid a where
    mempty :: a
    mppend :: a -> a -> a

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

-------------------------------------------------------

Monoids are required to satisfy the following identity and associativity laws:

mempty 'mappend' x = x

x 'mappend' mempty = x

x 'mappend' (y 'mappend' z) = (x 'mappend' y) 'mappend' z

-------------------------------------------------------

instance Monoid [a] where
    -- mempty :: [a]
    mempty = []

    -- mappend :: [a] -> [a] -> [a]
    mappend = (++)

instance Monoid a => Monoid (Maybe a) where
    -- mempty :: Maybe a
    mempty = Nothing

    -- mappend :: Maybe a -> Maybe a -> Maybe a
    Nothing `mappend` my = my
    mx `mappend` Nothing = mx
    Just x `mappend` Just y = Just (x `mappend` y)
-------------------------------------------------------
User defined:

instance Monoid Int where
    -- mempty :: Int
    mempty = 0

    -- mappend :: Int -> Int ->  Int
    mappend = (+)


newtype Sum a = Sum a
                deriving (Eq, Ord, Show, Read)

instance Monoid Int where
    -- mempty :: Int
    mempty = 1

    -- mempty :: Int -> Int -> Int
    mappend = (*)
-------------------------------------------------------
getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Monoid (Sum a) where
    -- mempty :: Sum a
    mempty = Sum 0

    -- mappend :: Sum a -> Sum a -> Sum a
    Sum x `mappend` Sum y = Sum (x+y)



> mconcat [Sum 2, Sum 3, Sum 4]
Sum 9

> Sum 0
Sum 0

> Sum 0.0
Sum 0.0

> Sum (-1.5)
Sum (-1.5)

-------------------------------------------------------

newtype Product a = Product a
                    deriving (Eq, Ord, Show, Read)

getProduct :: Product a -> a
getProduct (Product x) = x

instance Num a => Monoid (Product a) where
    -- mempty :: Product a
    mempty = Product 1

    -- mappend :: Product a -> Product a  -> Product a
    Product x `mappend` Product y = Product (x*y)

> mconcat [Product 2, Product 3, Product 4]



> mconcat [All True, All True, All True]
All {getAll = True}

> mconcat [Any False, Any False, Any False]
Any {getAny = False}

> mconcat [Any False, Any False, Any True]
Any {getAny = True}

> mconcat [All True, All True, All False]
All {getAll = False}

-}


-- fold :: Monoid a => [a] -> a
-- fold [] = mempty
-- fold (x:xs) = x `mappend` fold xs


{-



> fold [Sum 2, Sum 3, Sum 4]
Sum {getSum = 9}

> fold [Product 2, Product 3, Product 4]
Product {getProduct = 24}

> fold [All True, All True, All True]
All {getAll = True}

-}

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

fold' :: Monoid a => Tree a -> a
fold' (Leaf x) = x
fold' (Node l r) = fold' l `mappend` fold' r


{- 




> fold' $ Node (Leaf (Sum 3)) (Leaf (Sum 5))
Sum {getSum = 8}

-------------------------------------------------------
class Foldable t where
    fold :: Monoid a => t a -> a
    foldMap :: Monoid b => (a -> b) -> t a -> b
    foldr :: (a -> b -> b) => b -> t a -> b
    foldl :: (a -> b -> a) -> a -> t b -> a

instance Foldable [] where
    -- fold :: Monoid a -> [a] -> a
    fold [] = mempty
    fold (x:xs) = x `mappend` fold xs

    -- foldMap :: Monoid b => (a -> b) -> [a] -> b
    foldMap _ [] = mempty
    foldMap f (x:xs) = f x `mappend` foldMap f xs

    -- foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ v [] = v
    foldr f v (x:xs) = f x (foldr f v xs)

    -- foldr :: (a -> b -> a) -> a -> [b] -> a
    foldl _ v [] = v
    foldl f v (x:xs) = foldl f (f v x) xs

> getSum (foldMap Sum [1..10])
55

> getProduct (foldMap Product [1..10])
3628800

 
-}


instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold (Leaf x) = x
    fold (Node l r) = fold l `mappend` fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap f (Leaf x) = f x
    foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f v (Leaf x) = f x v
    foldr f v (Node l r) = foldr f (foldr f v r) l

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl f v (Leaf x) = f v x
    foldl f v (Node l r) = foldl f (foldl f v l) r


tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

{-

> foldr (+) 0 tree
6

> foldl (+) 0 tree
6

-------------------------------------------------------

The Foldable class includes:

null    :: t a -> Bool
length  :: t a -> Int
elem    :: Eq a => a -> t a -> Bool
maximum :: Ord a => t a -> a
minimum :: Ord a => t a -> a
sum     :: Num a => t a -> a
product :: Num a => t a -> a

> null []
True

> null (Leaf 1)
False

> length [1..10]
10

> length (Node (Leaf 'a') (Leaf 'b'))
2

Also versions of foldr and foldl for structures that 
contain at least one element, and hence do not require
a starting value:

foldr1 :: (a -> a -> a) -> t a -> a
foldl1 :: (a -> a -> a) -> t a -> a

> foldr1 (+) [1..10]
55

> foldl1 (+) [1..10]
55

> foldl1 (+) (Node (Leaf 1) (Leaf 2))
3

The final primitive in the class flattens a data structure
to a list:

toList :: t a -> [a]

> toList (Node (Leaf 1) (Leaf 2))
[1,2]

-------------------------------------------------------

In fact, the function toList plays a special role in the
declaration of the Foldable class, as it can be used to 
provide default definitions for most of the other 
primitives in the class in terms of the corresponding 
primitives for lists. In particular, we have the following
collection of default definitions:

foldr f v = foldr f v . toList
foldl f v = foldl f v . toList
foldr1 f  = foldr1 f . toList
foldl1 f  = foldl1 f . toList

null      = null . toList
length    = length . toList
elem x    = elem x . toList
maximum   = maximum . toList
minimum   = minimum . toList
sum       = sum . toList
product   = product . toList

For example, the definition null = null . toList states
that we can decide if a data structure is empty by first
flattening the structure to a list, and then checking if
this list is empty using the instance of null for lists.

The final three default definitions in the foldable class
establish importand relationships between the primitives
fold, foldMap and toList:

fold = foldMap id
foldMap f = foldr (mappend . f) mempty
toList = foldMap (\x -> [x])

------------------ Generic Functions ------------------


-}

average :: [Int] -> Int
average ns = sum ns `div` length ns

average' :: Foldable t => t Int -> Int
average' ns = sum ns `div` length ns

{-

average' can be applied to both lists and trees


> average' [1..10]
5
 
> average' (Node (Leaf 1) (Leaf 3))
2

-------------------------------------------------------

and :: Foldable t => t Bool -> Bool
and = getAll . foldMap All

or :: Foldable t => t Bool -> Bool
or = getAny . foldMap Any

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll . foldMap (All . p)

any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny . foldMap (Any . p)


> and (Node (Leaf True) (Leaf False))
False

> and [True,False,True]
False

> or (Node (Leaf True) (Leaf False))
True

> all even [1,2,3]
False

> any even (Node (Leaf 1) (Leaf 2))
True

-------------------------------------------------------

As a final example, the function concat :: [[a]] -> [a]
that concatenates a list of lists can now be generalised
to any foldable type whose elements are lists by simply 
folding the elements using the list Monoid:

concat :: Foldable t => t [a] -> [a]
concat = fold

> concat ["ab", "cd", "ef"]
"abcdef"

> concat (Node (Leaf [1,2]) (Leaf [3]))
[1,2,3]

------------------ Traversables -----------------------

-}

class Functor f where
    fmap :: (a -> b) -> f a -> f b

{-


map :: (a -> b) -> [a] -> [b]
map g [] = []
map g (x:xs) = g x : map g xs

traverse :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse g [] = pure []
traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs

-}

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

{-

> dec 10
Just 9

> traverse dec [1,2,3]
Just [0,1,2]

> traverse dec [2,1,0]
Nothing

-------------------------------------------------------

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a ->  f (t b)


-}








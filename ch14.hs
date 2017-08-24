{- Chapter 14.5 Exercises -}

import Data.Monoid
import Data.Foldable

{- 1. 

Commented out for 'Duplicate instance declarations'

instance (Monoid a, Monoid b) => Monoid (a,b) where
	-- mempty :: (a,b)
	mempty = (mempty, mempty)

	-- mappend :: (a,b) -> (a,b) -> (a,b)
	(x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)

-}

{- 2. 

instance Monoid b => Monoid (a -> b) where
	-- mempty :: (a,b)
	mempty = |_ -> mempty

	-- mappend :: (a,b) -> (a,b) -> (a,b)
	f `mappend` g = x -> f x `mappend` g x 


-}

{- 3. -}

{- 4. -}

{- 5. -}

data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a)
              deriving Show


{-

> (Node' (Leaf' ) (Just 5)  (Leaf'))
Node' Leaf' (Just 5) Leaf'

> Node' (Node' Leaf' (Just 10) Leaf') (Just 15) (Node' Leaf' (Just 20) Leaf')
Node' (Node' Leaf' (Just 10) Leaf') (Just 15) (Node' Leaf' (Just 20) Leaf')

-}


instance Foldable Tree' where
    -- fold :: Monoid a => Tree a -> a
    fold (Leaf') = mempty
    fold (Node' l a r) = fold l `mappend` fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap f (Leaf') = mempty
    foldMap f (Node' l a r) = foldMap f l `mappend` (f a) `mappend` foldMap f r

--     -- foldr :: (a -> b -> b) -> b -> Tree a -> b
--     foldr f v (Leaf x) = f x v
--     foldr f v (Node l r) = foldr f (foldr f v r) l

--     -- foldl :: (a -> b -> a) -> a -> Tree b -> a
--     foldl f v (Leaf x) = f v x
--     foldl f v (Node l r) = foldl f (foldl f v l) r

-- instance Functor Tree where
--     -- fmap :: (a -> b) -> Tree a -> Tree b
--     fmap g (Leaf x) = Leaf (g x)
--     fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- instance Traversable Tree where
--     -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
--     traverse g (Leaf x)   = pure Leaf <*> g x
--     traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r

{-

> filterF (> Just 10) (Node' (Node' Leaf' (Just 10) Leaf') (Just 15) (Node' Leaf' (Just 20) Leaf'))
[Just 15,Just 20]

-}


{- 6. -}

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

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

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g (Leaf x)   = pure Leaf <*> g x
    traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r


filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = filter f . toList

{-

> filterF (> 10) [1..20]
[11,12,13,14,15,16,17,18,19,20]

> filterF (< Just 20) (Node (Node (Leaf (Just 10)) (Leaf (Just 15))) (Leaf (Just 20)))
[Just 10,Just 15]

> filterF (/= Just 2) (Node (Node (Leaf (Just 1)) (Leaf (Just 2))) (Leaf (Just 1)))
[Just 1,Just 1]



-}
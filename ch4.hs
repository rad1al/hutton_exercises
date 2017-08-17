-- 4.8 Exercises.

{- 1. -}


halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs | not (even $ length xs) = error "list not even-length"
         | otherwise         = splitAt ((length xs) `div` 2) xs


{- 2. -}

-- using head and tail
third :: [a] -> a
third xs = head $ tail $ tail xs

-- using list indexing !!
third' :: [a] -> a
third' xs = xs !! 2

-- using pattern matching
third'' :: [a] -> a
third'' (_:_:x:_) = x


{- 3. -}

-- conditional expression
safetail :: [a] -> [a]
safetail xs = if null xs then []
              else tail xs

-- guarded equation
safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

-- pattern matching
safetail'' :: [a] -> [a]
safetail'' []     = []
safetail'' (_:xs) = xs

{- 4. 

False || False = False
True || True = True
True || False = True
False || True = True

False || False = False
_ || _         = True

False || b = b
True  || _ = True

a || b | a == b    = b
       | otherwise = True

-}

{- 5. -}

(&&) :: Bool -> Bool -> Bool
x && y = if x == True
         then if y == True
                 then True
                 else False
         else False

{- 6. -}

(&&&) :: Bool -> Bool -> Bool
a &&& b = if a == True
         then b
         else False

{- 7. -}

mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

mult' :: Int -> Int -> Int -> Int
mult' y z = \ x -> x * y * z

mult'' :: Int -> Int -> Int -> Int
mult'' z = \ x -> \ y -> x * y * z

mult''' :: Int -> Int -> Int -> Int
mult''' = \ x -> \ y -> \ z -> x * y * z


{- 8. -}

luhnDouble :: Int -> Int
luhnDouble x | 9 < (x * 2) = (x * 2) - 9
             | otherwise   = (x * 2)

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | total `mod` 10 == 0 = True
             | otherwise           = False
                where total = (luhnDouble a) 
                            + b 
                            + (luhnDouble c) 
                            + d











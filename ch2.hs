-- 2.7 Exercises.


{- 2. 
	2^3*4
=	(2^3)*4

	2*3+4*5
=	(2*3)+(4*5)

	2+3*4^5
=	2+(3*(4^5))
-}

{- 3. Fix and check for errors -}

-- n :: Int
-- n = a `div` length xs
--     where
--         a = 10
--         xs = [1,2,3,4,5]


{- 4. -}

last' []  = error "empty list"
last' xs  = drop ((length xs) - 1) xs

last'' []  = error "empty list"
last'' xs  = head $ reverse xs

last''' :: [a] -> a
last''' []  = error "empty list"
last''' [x] = x
last''' (x:xs) = last''' xs


{- 5. -}

take (length-1) [1,2,3,4,5]
take ((length [1,2,3,4,5])-1) [1,2,3,4,5]
reverse (drop 1 (reverse [1,2,3,4,5]))
(reverse . tail . reverse) [1,2,3,4,5]

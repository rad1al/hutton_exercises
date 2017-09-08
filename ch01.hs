-- 1.7 Exercises.


{- 1. 
	double (double 2)
= 	double (2 + 2)
= 	(2 + 2) + (2 + 2)
= 	4 + (2 + 2)
=	4 + 4
= 	8 
-}

{- 2.
	sum [x]
= 	x + sum []
=	x + 0
=	x
-}

{- 3.
	product' [2,3,4]
=	2 * (product' [3,4])
=	2 * (3 * product' [4])
=	2 * (3 * (4 * product' []))
=	2 * (3 * (4 * 1))
=	24

-}
product' [] = 1
product' (x:xs) = x * product' xs

{- 4. -}
reverseqsort [] = []
reverseqsort (x:xs) = reverseqsort larger ++ [x] ++ reverseqsort smaller
                      where
                        larger = [a | a <- xs, a >= x]
                        smaller = [b | b <- xs, b < x]

{- 5. 
Replacing <= by < in the original definition of qsort would remove duplicates 
from appearing in the output.
-}

qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
                where
                  smaller = [a | a <- xs, a < x]
                  larger = [b | b <- xs, b > x]
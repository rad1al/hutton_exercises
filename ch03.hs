-- 3.11 Exercises.

{- 1. 

['a','b','c'] :: [Char]

('a','b','c') :: (Char, Char, Char)

[(False,'O'),(True,'1')] :: [(Bool,char)]

([False,True],['0','1']) :: ([Bool], [Char])

[tail, init, reverse] :: [[a]->[a]]

-}

{- 2. -}

bools :: [Bool]
bools = [True]

nums :: [[Int]]
nums = [[1],[2],[3]]

add :: Int -> Int -> Int -> Int
add a b c = a+b+c

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply func x = func x

{- 3. 

second xs = head (tail xs)
:: [a] -> a

swap (x,y) = (y,x)
:: (t,t1) -> (t,t1)
* tuples have their own type

pair x y = (x,y)
:: t1 -> t -> (t1,t)

double x = x*2
:: Num a => a -> a

palindrome xs = reverse xs == xs
:: Eq a => [a] -> Bool

twice f x = f (f x)
:: (t -> t) -> t -> t

-}

{- 4. Checked #3 with GHCi. -}

{- 5. 

From Stephan202 (https://stackoverflow.com/users/74939/stephan202):

Given an arbitrary function, f, we define a function f' which returns 1 on input n if f halts on input n. 

Now, for some number x we define a function g which, on input n, returns 1 if n = x, and otherwise calls f'(n).

If functional equivalence were decidable, then deciding whether g is identical to f' decides whether f halts on input x. 

That would solve the Halting problem.


-}
{- Chapter 1 :: Introduction -}

{-

Features of Haskell:

- Concise programs
- Powerful type system
- List comprehensions
- Recursive functions
- Higher-order functions
- Effectful functions
- Generic functions
- Lazy evaluation
- Equational reasoning

-}

sum' [] = 0
sum' (n:ns) = n + sum' ns

{-

> sum' [1,2,3,4]
10

-}

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where 
                  smaller = [a | a <- xs, a <= x]
                  larger = [b | b <- xs, b > x]

{-

> qsort [4,5,2,1,3,6]
[1,2,3,4,5,6]

-}

seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

{-

The expression "seqn [getChar, getChar, getChar]" reads three
characters from the keyboard using the action "getChar" that reads a 
single character, and returns a list containing the three characters. 

> seqn [getChar, getChar, getChar]
abc"abc"

-}
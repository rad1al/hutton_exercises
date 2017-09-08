{- Chapter 2 :: First steps -}

{-

> 2+3*4
14

> (2+3)*4
20

> sqrt (3^2 + 4^2)
5.0

------------------ Standard prelude functions -----------------

- head
- tail
- take
- drop
- length
- sum
- product
- ++
- reverse


-}

double x = x + x

quadruple x = double (double x)

{-

> quadruple 10
40

------------------- Keywords -----------------

These cannot be used as the names of functions or their arguments:

case, class, data, default, deriving, do, else, foreign, if, import,
in, infix, infixl, infixr, instance, let, module, newtype, of, then,
type, where

-}

-- Factorial of a positive integer:
factorial n = product [1..n]

-- Average of a list of integers:
average ns = sum ns `div` length ns

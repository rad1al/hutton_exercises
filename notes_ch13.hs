{- Chapter 13 :: Monadic parsing -}

-- data Tree a = Leaf a | Node (Tree a) a (Tree a)
              -- deriving Show

-- type Parser = String -> Tree

-- type Parser = String -> (Tree,String)

-- type Parser = String -> [(Tree,String)]

-- type Parser a = String -> [(a,String)]

import Control.Applicative
import Data.Char
import System.IO

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])

{-

> parse item ""
[]

> parse item "abc"
[('a',"bc")]

-}

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            [] -> []
                            [(v,out)] -> [(g v, out)])

{-

> parse (fmap toUpper item) "abc"
[('A',"bc")]

> parse (fmap toUpper item) ""
[]


-}

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                             [] -> []
                             [(g,out)] -> parse (fmap g px) out)



{-

> parse (pure 1) "abc"
[(1,"abc")]

-}

three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x,z)


{-

> parse three "abcdef"
[(('a','c'),"def")]

Note that the applicative machinery automatically ensures that the above parser fails
if the input string is too short, without the need to detect or manage this ourselves:

> parse three "ab"
[]


-}

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                           [] -> []
                           [(v,out)] -> parse (f v) out)
{-

The parser p >>= f fails if the application of the parser p to the input string inp
fails, and otherwise applies the function f to the result value v to give another 
parser f v which is then applied to the output string out that was produced by the first
parser to give the final result. 

Because Parser is a monadic type, the do notation can now be used to sequence parsers
and process their result values. For example, the parser three can be defined in an 
alternative manner as follows:

-}

three' :: Parser (Char,Char)
three' = do x <- item
            item 
            z <- item
            return (x,z) -- return is just another name for the applicative function
                         -- pure, which in this case builds parsers that always succeed


{-

> parse three' "abcdef"
[(('a','c'),"def")]

----------------- Making choices ----------------------

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a


Identity and associativity rules:

empty <|> x     = x
x <|> empty     = x
x <|> (y <|> z) = (x <|> y) <|> z

instance Alternative Maybe where
    -- empty :: Maybe a
    empty = Nothing

    -- (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> my = my
    (Just x) <|> _ = Just x

-}

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                            [] -> parse q inp
                            [(v,out)] -> [(v,out)])


{-

> parse empty "abc"
[]

> parse (item <|> return 'd') "abc"
[('a',"bc")]

> parse (empty <|> return 'd') "abc"
[('d',"abc")]

--------------- Derived Primitives --------------------


-}

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x
             then return x
           else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

{-

> parse (char 'a') "abc"
[('a',"bc")]

> parse (char 'b') "abc"
[]

-}


string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


{-

> parse (string "abc") "abcdef"
[("abc","def")]

> parse (string "ab") "abcdef"
[("ab","cdef")]

> parse (string "abc") "ab1234"
[]

> parse (string "xyz") "abcdef"
[]

> parse (many digit) "123abc"
[("123","abc")]

> parse (many digit) "abc"
[("","abc")]

> parse (some digit) "abc"
[]

-------------------------------------------------------

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    many :: f a -> f [a]
    some :: f a -> f [a]

    many x = some <|> pure []
    some x = pure (:) <*> x <*> many x

-}

-- parsers for identifiers (variable names):

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

{-

> parse ident "abc def"
[("abc"," def")]

> parse nat "123 def"
[(123," def")]

> parse nat "def 123"
[]

> parse space " abc"
[((),"abc")]


-}

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|> nat

{-

> parse int "-123 abc"
[(-123," abc")]

> parse int "123 abc"
[(123," abc")]

> parse int "1x3 abc"
[(1,"x3 abc")]

--------------- Handling spacing --------------------

Most real-life parses allow spacing to be freely used
around the basic tokens in their input string, i.e.
1+2 and 1 + 2 are both parsed in the same way by GHC.

-}

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol "," 
                         natural)
          symbol "]"
          return (n:ns)


{-

> parse nats " [1,2,3] "
[([1,2,3],"")]

> parse nats " [1, 2,3 ] "
[([1,2,3],"")]

> parse nats "[1,2,]"
[]

This definition states that such a list begins with an
opening square bracket and a natural number, followed by
zero or more commas and natural numbers, and concludes 
with a closing square bracket.

Note that nats only succeeds if a complete list in 
precisely this format is consumed.

--------------- Arithmetic expressions ----------------

grammar = set of rules that describes how strings of the
language can be constructed.

Original grammar:

expr ::= expr * expr | expr * expr | ( expr ) | nat
nat  ::= 0 | 1 | 2 | ...

Final grammar:

expr    ::= term (+ expr | e) where e == empty string
term    ::= factor (* term | e)
factor  ::= factor (* term | e)
nat     ::= 0 | 1 | 2 | ...

-}


expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
             <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n,[])]  -> n
            [(_,out)] -> error ("Unused input " ++ out)
            []        -> error "Invalid input"

{-

> eval "2*3+4"
10

> eval "2*(3+4)"
14

> eval "2*3^4"
*** Exception: Unused input ^4

> eval "one plus two"
*** Exception: Invalid input

--------------- Calculator ----------------

q = quit
c = clear the display
d = delete a character
(=) evaluate an expression

The remaining buttons allow the user to 
enter expressions.

-}


box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"


showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

-- Chapter 10 input/output utilities:

-- clear screen --
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-------------------------------------------

beep :: IO ()
beep = putStr "\BEL"

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                process c xs
             else
                do beep
                   calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval' xs
             | elem c " cC"       = clear
             | otherwise          = press c xs


-- quitting moves the cursor below the calculator 
-- box and terminates.
quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval' :: String -> IO ()
eval' xs = case parse expr xs of 
             [(n,[])] -> calc (show n)
             _        -> do beep
                            calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear








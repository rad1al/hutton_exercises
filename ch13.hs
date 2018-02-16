{- Chapter 13.11 Exercises -}

import Data.Char
import Control.Applicative

{- 1. 

Define a parser comment :: Parser () for ordinary Haskell comments that begin with the
symbol -- and extend to the end of the current line, which is represented by the control
character '\n'.

-}

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            [] -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                             [] -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                           [] -> []
                           [(v,out)] -> parse (f v) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                            [] -> parse q inp
                            [(v,out)] -> [(v,out)])

    -- See: http://hackage.haskell.org/package/base-4.10.1.0/docs/src/GHC.Base.html#many
    -- many x = some x <|> pure []
    -- some x = pure (:) <*> x <*> many x

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x
             then return x
           else empty

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)                    

comment :: Parser ()
comment = do string "--"
             many (sat (/= '\n'))
             char '\n'
             return ()

{-

> parse comment "--comment \nsome code"
[((),"some code")]

> parse comment "comment \nsome code"
[]

-}


{- 2. 
                                                                        
                  expr                            expr                  
                ┌─ │  ─┐                        ┌─ │  ─┐                
              ┌─┘  │   └─┐                    ┌─┘  │   └─┐              
            ┌─┘    │     └─┐                ┌─┘    │     └─┐            
          ┌─┘      │       └─┐            ┌─┘      │       └─┐          
          ▼        ▼         └▼           ▼        ▼         └▼         
        expr       +         expr        expr      +         expr       
       ┌─┐─┐                  │           │                 ┌─┐─┐       
     ┌─┘ │ └─┐                │           │               ┌─┘ │ └─┐     
   ┌─┘   │   └─┐              │           │             ┌─┘   │   └─┐   
  ▼┘     ▼     ▼              ▼           ▼            ▼┘     ▼     ▼   
 expr    +    expr           term        term         expr    +    expr 
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  ▼            ▼              ▼           ▼           ▼             ▼   
 term         term          factor      factor       term          term 
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  ▼            ▼              ▼           ▼           ▼             ▼   
factor       factor          nat         nat         factor       factor
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  ▼            ▼              ▼           ▼           ▼             ▼   
 nat          nat             4           2          nat           nat  
  │            │                                      │             │   
  │            │                                      │             │   
  │            │                                      │             │   
  ▼            ▼                                      ▼             ▼   
  2            3                                      3             4   
                                                                        

-}

{- 3. -}
-- To be implemented.

{- 4. -}
-- To be implemented.

{- 5. -}
-- To be implemented.

{- 6. -}

digit :: Parser Char
digit = sat isDigit

-- See isSpace at: https://hackage.haskell.org/package/base-4.10.1.0/docs/src/GHC.Unicode.html#isSpace
space :: Parser ()
space = do many (sat isSpace)
           return ()

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             <|> do symbol "-"
                    e <- expr
                    return (t - e)
                    <|> return t

term :: Parser Int
term = do ep <- factor
          do symbol "*"
             t <- term
             return (ep * t)
             <|> do symbol "/"
                    t <- term
                    return (ep `div` t)
                    <|> return ep

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> integer

eval :: String -> Int
eval xs = case (parse expr xs) of
               [(n, [])]  -> n
               [(_, out)] -> error ("Unused input " ++ out)
               []         -> error "Invalid input"
               _          -> undefined

{-

> eval "(2*9)-5"
13

> eval "(2*9)-"
*** Exception: Unused input -

> eval "*5(2*9)"
*** Exception: Invalid input

> eval "9/0"
*** Exception: divide by zero

-}

{- 7. -}
-- To be implemented.

{- 8. 

-- a.

expr ::= expr - nat | nat
nat  ::= 0 | 1 | 2 | ...

-}

-- b.

natural :: Parser Int
natural = token nat

expr' :: Parser Int
expr' = do e <- expr
           do symbol "-"
              n <- natural
              return (e-n)
              <|> return e
              -- <|> natural -- Not sure if this is correct, based off factor in notes.

-- c.
-- Since the first thing it does is recursively call itself, the parser will loop forever and never produce a result. 


-- d.

expr'' :: Parser Int
expr'' = do n <- natural
            ns <- many (do symbol "-"
                           natural)
            return $ foldl (-) n ns

{- 9. -}
-- To be implemented.


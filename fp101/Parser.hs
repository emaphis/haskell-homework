-- | Parser Homework

module Parser where

import Control.Applicative
import Control.Monad
import Data.Char

infixr 5 +++

newtype Parser a   = P (String -> [(a,String)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

-- return   :: a -> Parser a
-- (>>=)   :: Parser a -> (a -> Parser b) -> Parser b
instance Monad Parser where
  return v     = P (\ inp -> [(v,inp)])
  p >>= f      = P (\inp -> case parse p inp of
                             []        -> []
                             [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
  mzero        = P (\inp -> [])
  p `mplus` q  = P (\inp -> case parse p inp of
                             []         -> parse q inp
                             [(v,out)]  -> [(v,out)])

instance Alternative Parser where
   empty      =  P (\inp -> [])
   p <|> q    =  P (\inp -> case parse p inp of
                             []        -> parse q inp
                             [(v,out)] -> [(v,out)])


-- The parser item fails if the input is empty, and consumes
-- the first character otherwise
item  :: Parser Char
item   = P (\ inp -> case inp of
  []     -> []
  (x:xs) -> [(x,xs)])

-- The parser failure always fails:
failure  :: Parser a
failure   =  P (\ inp -> [])  -- mzero


-- behaves like a parser p if p succeeds, parser q otherwise
(+++)   :: Parser a -> Parser a -> Parser a
p +++ q  = p `mplus` q

-- parse applies a parser to a string
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp


-- parsing a char that satisfies a predicate
sat  ::  (Char ->  Bool) -> Parser Char
sat p = do x <- item
           if p x
             then return x
             else failure

digit   :: Parser Char
digit    = sat isDigit

lower   :: Parser Char
lower    = sat isLower

upper   :: Parser Char
upper    = sat isUpper

letter   :: Parser Char
letter    = sat isAlpha

alphanum :: Parser Char
alphanum  = sat isAlphaNum

char     :: Char -> Parser Char
char x    = sat (== x)

-- apply a parser zero or more times
many0   :: Parser a -> Parser [a]
many0 p  = many1 p +++ return []

-- apply a psrser one or more times
many1  :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many0 p
             return (v:vs)

-- parsing a specific string of characters
string       :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident        :: Parser String
ident         =  do x  <- lower
                    xs <- many1 alphanum
                    return (x:xs)

nat          :: Parser Int
nat           = do xs <- many1 digit
                   return (read xs)


-- Exercise

-- Ex (1) integer parser
int :: Parser Int
int  = do _ <-char '-'
          n <- nat
          return (-n)
        +++ nat

-- Ex (2) -- deine comment parser

comment :: Parser ()
comment = do string "--"
             many1 (sat (/= '\n'))
             return ()

-- Ex (3)  >>= bind

bind :: Parser a -> (a -> Parser a) -> Parser a
bind p f = P (\inp -> case parse p inp of
                       []  -> []
                       [(v,out)] -> parse (f v) out)

-- | Lecture 7 -- Functional Parsers
-- | Book Chapter 8

module Lecture07 where

import Data.Char
import Control.Monad

infixr 5 +++

-- 7.1  Parsers

-- a parser is a program that analyses a piece of text to
-- determine its syntactic structure.

-- almost every real life programm uses some form of parser
-- to pre-process its input.


-- 7.2  The parser type

--type Parser = String -> Tree

-- use list instead of Maybe type:
newtype Parser a   = P (String -> [(a,String)])

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


-- 7.3  Basic parsers

-- The parser item fails if the input is empty, and consumes
-- the first character otherwise
item  :: Parser Char
item   = P (\ inp -> case inp of
  []     -> []
  (x:xs) -> [(x,xs)])

-- The parser failure always fails:
failure  :: Parser a
failure   = mzero

-- return always succeeds.
--return   :: a -> Parser a
--return v  = \ inp -> [(v,inp)]


-- 7.4  Sequencing

--(>>=)   :: Parser a -> (a -> Parser b) -> Parser b
--p >>= f  =
--  \inp -> case parse p inp of
--  [] -> []
--  [(v,out)] -> parse (f v) out


-- a parser that consumes three characters, discards
-- the second and returns the third as a pair
p1 :: Parser (Char,Char)
p1  = do x <- item
         _ <-item
         y <- item
         return (x,y)


-- 7.5  Choice

-- behaves like a parser p if p succeeds, parser q otherwise
(+++)   :: Parser a -> Parser a -> Parser a
p +++ q  = p `mplus` q

-- parse applies a parser to a string
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp


--7.6  Derived primitives

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
many   :: Parser a -> Parser [a]
many p  = many1 p +++ return []

many1  :: Parser a -> Parser [a]
many1 p = do v  <-p
             vs <- many p
             return (v:vs)

-- parsing a specific string of characters
string       :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


ident        :: Parser String
ident         =  do x  <- lower
                    xs <- many alphanum
                    return (x:xs)

nat          :: Parser Int
nat           = do xs <- many1 digit
                   return (read xs)

-- Ex 8.1
-- Ex (6)
int,int1,int3,int4  :: Parser Int
int           = do char '-'
                   n <- nat
                   return (-n)
                 +++ nat

-- bad
int1 = char '-' >>= (\ c -> nat >>= (\ n -> (return (-n) +++ nat)))

-- won't compile
--int2 = (nat +++ char '-') >>= (\ c -> nat >>= (\ n -> return (-n)))

-- good!
int3 = (do char '-'
           n <- nat
           return (-n))
         +++ nat

-- bad out put ignores '-'
int4 = (do char '-'
           nat)
         +++ nat

space        :: Parser ()
space         = do many (sat isSpace)
                   return ()

-- Ex 8.2
-- Ex (7)
comment  :: Parser ()
comment       = do string "--"
                   many (sat (/= '\n'))
                   return ()

expr'        :: Parser Int
expr'         = error "You must implement expr"


-- Example - a parser that consumes a list of one
-- or more digits from a string
p2  :: Parser String
p2   = do char '['
          d  <- digit
          ds <- many (do char ','
                         digit)
          _ <- char ']'
          return (d:ds)


-- 7.7  Handling spacing

token        :: Parser a -> Parser a
token p        = do space
                    v <- p
                    space
                    return v

identifer    :: Parser String
identifer     = token ident

natural      :: Parser Int
natural       = token nat

integer      :: Parser Int
integer       = token int

symbol       :: String -> Parser String
symbol xs     = token (string xs)


-- a parser for a non-empty list of natural numbers that
-- ignores spacing around tokens

p3   :: Parser [Int]
p3    = do symbol "["
           n  <- natural
           ns <- many (do symbol ","
                          natural)
           symbol "]"
           return (n:ns)

-- 7.8  Arithmetic expressions
{-
expr -> term '+' expr | term

term -> factor '*' term | factor

factor -> digit | '(' expr ')'

digit -> '0' | '1' | ... | '9'
-}

expr  :: Parser Int
expr   = do t <- term
            do char '+'
               e <- expr
               return (t + e)
              +++ return t

term :: Parser Int
term  = do f <- factor
           do char '*'
              t <- term
              return (f * t)
             +++ return f

factor :: Parser Int
factor  = do d <- digit
             return (digitToInt d)
          +++ do char '('
                 e <- expr
                 char ')'
                 return e

eval  :: String -> Int
eval xs = fst (head (parse expr xs))



-- Exercises:


-- Ex (0)
pair0 = parse item "hello"  -- => [('h',"ello")]


-- Ex (1)
parse1 :: Parser Integer
parse1 = return 1 +++ return 2

pair1 = parse parse1 "abc"  -- => [(1,"abc")]


-- Ex (2)
pair2 :: [(Integer, String)]
pair2 = parse (return 1) "hello"  -- => [(1,"hello")]


-- Ex (3)
pair3 = parse (item +++ return 'a') "hello"
-- => [('h',"ello")]

-- Ex (4)  -- and implementation of (>>=)
 -- p >>= f
 --   = P (\inp ->
 --         case parse p inp of
 --             []        -> []
 --             [(v,out)] -> parse (f v) out)


-- Ex (5)
-- parser : char 'a' +++ return 'b'
-- always succeeds

-- Ex (6) -- int - see above in

-- Ex (7) -- comment - see above

-- Ex (8) -- expression - see above

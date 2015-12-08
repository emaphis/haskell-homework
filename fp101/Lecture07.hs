-- | Lecture 7 -- Functional Parsers
-- | Book Chapter 8

module Lecture07 where

import Control.Applicative
import Control.Monad
import Data.Char

infixr 5 +++

-- 7.1  Parsers

-- a parser is a program that analyses a piece of text to
-- determine its syntactic structure.

-- almost every real life programm uses some form of parser
-- to pre-process its input.


-- 7.2  The parser type

--type Parser = String -> Tree

-- unpeel the onion:

-- a parser may not parse all of it's input
-- type Parser = String -> (Tree,String)

-- a string might be parsable in many ways, including none
-- type Parser1 = String -> Maybe (Tree,String)

-- but generalize to use a list instead of Maybe type:
-- type Parser = String -> [(Tree,String)]

-- a parser may not always return a tree
newtype Parser a   = P (String -> [(a,String)])

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

-- Need the Functor and Applicative instances too!
-- This is generic code that should work for any Monad
instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Alternative Parser where
   empty      =  P (\inp -> [])
   p <|> q    =  P (\inp -> case parse p inp of
                             []        -> parse q inp
                             [(v,out)] -> [(v,out)])

-- 7.3  Basic parsers

-- The parser item fails if the input is empty, and consumes
-- the first character otherwise
item  :: Parser Char
item   = P (\ inp -> case inp of
  []     -> []
  (x:xs) -> [(x,xs)])

-- The parser failure always fails:
failure  :: Parser a
failure   =  P (\ inp -> [])  -- mzero

-- return always succeeds.
-- return   :: a -> Parser a
-- return v  = P (\ inp -> [(v,inp)])


-- 7.4  Sequencing

-- bind, select many
--(>>=)   :: Parser a -> (a -> Parser b) -> Parser b
--p >>= f  =
--  \inp -> case parse p inp of
--  [] -> []
--  [(v,out)] -> parse (f v) out


-- A sequence of parsers can be combined as a single
-- composite parser using the keyword 'do'
-- a parser that consumes three characters, discards
-- the second and returns the third as a pair
p1 :: Parser (Char,Char)
p1  = do x <- item
         _ <- item
         y <- item
         return (x,y)

p1' :: Parser (Char,Char)
p1' = item >>= \x ->
      item >>= \_ ->
      item >>= \y ->
      return (x,y)

str0  = parse p1 "abcdef"
-- [(('a','c'),"def")]

stra = parse p1 "ab"  -- failure
-- []

strb  = parse p1 "1 2 3 4 5"
-- [(('1','2')," 3 4 5")]


-- 7.5  Choice

-- behaves like a parser p if p succeeds, parser q otherwise
(+++)   :: Parser a -> Parser a -> Parser a
p +++ q  = p `mplus` q
 -- p +++ q  = P (\inp -> case parse p inp of
 --                         []         -> parse q inp
 --                         [(v,out)]  -> [(v,out)])

-- parse applies a parser to a string
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp


-- Examples
str1  = parse item ""
-- []

str2  = parse item "abc"
-- [('a',"bc")]

str3  = parse failure "abc"
-- []

str4  = parse (return 1) "abc"
-- [(1,"abc")]

str5  = parse (item +++ return 'd') "abc"
-- [('a',"bc")]

str6  = parse (failure +++ return 'd') "abc"
-- [('d',"abc")]


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

lst10 = parse digit "123"
-- [('1',"23")]

lst11 = parse digit "abc"
-- []

lst12 = parse (char 'a') "abc"
-- [('a',"bc")]

lst13 = parse (char 'a') "def"
-- []


-- apply a parser zero or more times
many0   :: Parser a -> Parser [a]
many0 p  = many1 p +++ return []

-- apply a psrser one or more times
many1  :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many0 p
             return (v:vs)

lst07 = parse (many0 digit) "123abc"
-- [("123","abc")]


lst08 = parse (many0 digit) "abcdef"
-- [("","abcdef")]

lst09 = parse (many1 digit) "abcdef"
-- []


-- parsing a specific string of characters using char parser
string       :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

lst14 = parse (string "abc") "abcdef"
-- [("abc","def")]

lst15 = parse (string "abc") "ab1234"
-- []


-- consume a list of one or more digits
-- from a string
p4  :: Parser String
p4 = do char '['
        d  <- digit
        ds <- many0 (do char ','
                        digit)
        char ']'
        return (d:ds)

str7  = parse p4 "[1,2,3,4]"
-- [("1234","")]

str8  = parse p4 "[2,3,4"  -- failure
-- []


-- parsers using many
ident        :: Parser String
ident         =  do x  <- lower
                    xs <- many1 alphanum
                    return (x:xs)

nat          :: Parser Int
nat           = do xs <- many1 digit
                   return (read xs)

space        :: Parser ()
space         = do many0 (sat isSpace)
                   return ()

lst16 = parse ident "abc def"
-- [("abc"," def")]

lst17 = parse nat "123 abc"
-- [(123," abc")]

lst18 = parse space "    abc"
-- [((),"abc")]

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


-- Ex 8.2
-- Ex (7)
comment  :: Parser ()
comment       = do string "--"
                   many1 (sat (/= '\n'))
                   return ()

expr'        :: Parser Int
expr'         = error "You must implement expr"



-- 7.7  Handling spacing
-- using parser space we can write paresers that ignore spacing

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

-- a parser for a non-empty list of natural numbers that ignores
-- spacing around tokens
p3   :: Parser [Int]
p3    = do symbol "["
           n  <- natural
           ns <- many0 (do symbol ","
                           natural)
           symbol "]"
           return (n:ns)

lst19 = parse p3 "  [1,  2, 3] "
-- [([1,2,3],"")]

lst20 = parse p3 "[1,2,]"
-- []


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

str9  = eval "2*3+4"   -- 10

str10 = eval "2*(3+4)" -- 14


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

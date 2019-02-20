{-# LANGUAGE InstanceSigs #-} -- allows type signatures in instances

module Parser where

import Data.Char (toUpper, isDigit)
import Control.Applicative

-- code based on 
--   https://eli.thegreenplace.net/2017/deciphering-haskells-applicative-and-monadic-parsers/
--   https://blog.lahteenmaki.net/applicative-parsing.html


--------------
-- Parser Type
--------------

-- A parser is a function from a string to a list of value-string pairs.
-- Each pair in the list represents a partial parse of the input string, and 
-- the remaining string after that parse, and an empty list represents a failed parse.

newtype Parser a = Parser { getParser :: (String -> [(a,String)]) }


------------------
-- Parser Intances
------------------

-- the functor instance allows us to create new parsers by applying
-- functions to the values they parse

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g (Parser px) = Parser $ \input ->
      [(g x, out) | (x, out) <- px input]

-- ghci> getParser (fmap toUpper item) "hello"


-- The applicative instance allows sequencing parsers, by taking parsed
-- functions and applying them to subsequent parsed values. This also allows
-- lifting functions into the parser context, thereby combining parsers.

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \inp -> [(x,inp)] -- lifts a value to a parsed value without consuming anything

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser px = Parser $ \input ->
      [(f a, output) | (f, intermediate) <- pf input,
                       (a, output) <- px intermediate]

-- one simple example of how applicative is useful uses two functions it
-- provides, '(*>)' and '(<*)'. These are defined as
--     (<*) :: f a -> f b -> f a
--     (<*) = liftA2 const
--
--     (*>) :: f a -> f b -> f b
--     (*>) = liftA2 (flip const)
-- these functions simply lift the functions of two arguments that return
-- their first and second argument respectively. However, since they are
-- being applied in the applicative context, whatever effects they produce
-- are still applied without their return values being used. In the context
-- of parsing, this means that, for example, (*>) consumes whatever is matched
-- both parsers passed to it, but returns only the result of the second parser

-- ghci> getParser (item *> item) "hello"
-- ghci> getParser (item <* item) "hello"


-- Next we implement an instance for Parser of the Alternative typeclass,
-- which builds on top of Applicative. You can read about it here:
-- https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus

-- The alternative instance allows running multiple parsers until
-- one succeeds.

instance Alternative Parser where
    empty :: Parser a  -- represents a failed parser, identity under <|>
    empty = Parser $ \_ -> []

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p) <|> (Parser q) = Parser $ \input ->
        case p input of
            [] -> q input  -- run second parser if first fails
            r -> r         -- if first parser succeeds, ignore second


-- ghci> getParser (char 'h' <|> char 'g') "hello"
-- ghci> getParser (char 'h' <|> char 'g') "gello"
-- ghci> getParser (char 'h' <|> char 'g') "kello"

-- we can also use two other functions provided by Alternative,
--   some :: f a -> f [a] -- transforms parser that captures a value of type 
--                           `a` to a parser that matches >= 1 values of type
--                           `a` and returns a list of those values until the 
--                           parser fails
--   many :: f a -> f [a] -- matches zero or more

-- ghci> getParser (many $ char 'a') "aaabcd"


----------------
-- Basic Parsers
----------------

-- captures the first character, fails on empty string
item :: Parser Char
item = Parser $ \input ->
    case input of
      []      -> []        -- fails on empty string
      (x:xs)  -> [(x,xs)]  -- non-empty string

-- ghci> getParser item "hello"

-- parser that captures the first character if it satisfied a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \input -> 
    case input of
      x:xs | pred x -> [(x,xs)] 
      _ -> []

-- ghci> getParser (satisfy (== 'h')) "hello"
-- ghci> getParser (satisfy (== 'g')) "hello"


-- parser that captures a specific character
char :: Char -> Parser Char
char = satisfy . (==) -- char c = satisfy (== c)


-- parser that captures anything but a specific character
notChar :: Char -> Parser Char
notChar = satisfy . (/=)

-- ghci> getParser (char 'h') "hello"
-- ghci> getParser (char 'g') "hello"

digit :: Parser Char
digit = satisfy isDigit

-- parser that captures >= 0 spaces
spaces :: Parser String
spaces = many $ char ' ' -- many takes a Parser Char and gives a Parser [Char]

-- ghci> getParser spaces "   hello"
-- ghci> getParser (spaces *> item) "   hello"

module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of
                 []         -> []
                 [(v,out)] ->  [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v,inp)])

  {- pg  :: Parser (a->b)
     px  :: Parser a
     inp :: String
     parse pg inp :: [((a->b),String)]
     g   :: a->b
     fmap :: (a->b) -> Parser a -> Parser b
     fmap g px :: Parser b
     parse (fmap g px) out :: Parser b -> String
     Hence [(b,String)]
     Final type : String -> [(b,String)] thus Parser b
  -}

  pg <*> px = P (\inp -> case parse pg inp of
                  []        -> []
                  [(g,out)] -> parse (fmap g px) out) 

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
                  []        -> []
                  [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\inp -> [])

  p <|> q = P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [(v,out)] -> [(v,out)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
             []     -> []
             (x:xs) -> [(x,xs)])

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x,z)

three' :: Parser (Char,Char)
three' = do x <- item
            item
            z <- item
            return (x,z)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

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
char x = sat ( == x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

{-
  many x = some x <|> pure []          zero or more
  some x = pure (:) <*> x <*> many x   one or more
-}

{-identifiers (variable names) comprising a lower-case letter
  followed by zero or more alphanumeric characters-}

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

--natural numbers comprising one or more digits
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

--spacing comprising zero or more space, tab, and newline characters
space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat


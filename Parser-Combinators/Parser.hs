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
three' = do
            x <- item
            item
            z <- item
            return (x,z)

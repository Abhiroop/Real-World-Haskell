{-# LANGUAGE OverloadedStrings #-}
module RandomExample where

import Data.Maybe
import System.Random
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Text as T
import Control.Applicative ((<|>))
data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq,Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

--readP_to_S (many $ satisfy (not . isUpper)) "ab/cD"
-- public java.util.ArrayList(java.util.Collection<? extends E>)
-- L(java.util.Collection<+TE;>)V

methodParser = do
  char 'L'
  char '('
  methodType <- (many $ satisfy (/= '<'))
  char '+' <|> char '-'

test = do
  x <- char 'L'
  y <- char 'C'
  char 'a'
  return $ show $ x:y:[]

-- L(java.util.Collection<+TE;>)V
parseRawSignature :: ReadP [Char]
parseRawSignature = do
  char 'L'
  char '('
  methodType <- (many $ satisfy (/= '<'))
  char '<'
  extendsOrSuper <- get
  char 'T'
  typeVariable <- get
  char ';'
  char '>'
  char ')'
  x <- look
  return x

--public String hello(String a, int a, int b)` -> `(Ljava/lang/String;II)Ljava/lang/String;

parseClassName :: ReadP [Char]
parseClassName = undefined


-----------------------------------------------------------
{-
1. Split method signature
2. parse parameter types
3. parse return types
-}
splitMethodSignature :: ReadP [Char]
splitMethodSignature = (between (char '(') (char ')') (many (satisfy (\c -> True))))

-- (Ljava/lang/String;II)
-- (TT;Ljava/util/List<TU;>;Ljava/util/ArrayList<TE;>;)
-- (TT;Ljava/util/List<-TX;>;Ljava/util/ArrayList<+TY;>;)
-- (Ljava/lang/Class<*>;)
parseParameterType :: ReadP [Char]
parseParameterType = (char 'L' >> parseReferenceType)
                 <|> many parsePrimitiveType

parsePrimitiveType :: ReadP Char
parsePrimitiveType = char 'I'
                 <|> char 'B'
                 <|> char 'Z'

parseSimpleRefType :: ReadP [Char]
parseSimpleRefType = do
  x <- many (satisfy (/= ';'))
  return x

parseSimpleTypeVariable :: ReadP [Char]
parseSimpleTypeVariable = do
  char 'T'
  typeVariable <- get
  return [typeVariable]

parseExtendsTypeVariable :: ReadP [Char]
parseExtendsTypeVariable = do
  char '+'
  char 'T'
  typeVariable <- get
  return $ "extends " ++ [typeVariable]

parseSuperTypeVariable :: ReadP [Char]
parseSuperTypeVariable = do
  char '-'
  char 'T'
  typeVariable <- get
  return $ "super " ++ [typeVariable]

parseWildCard :: ReadP [Char]
parseWildCard = do
  x <- char '*'
  return [x]

parseType :: ReadP [Char]
parseType = parseSimpleTypeVariable
        <|> parseExtendsTypeVariable
        <|> parseSuperTypeVariable
        <|> parseWildCard

parseGenericRefType :: ReadP [Char]
parseGenericRefType = do
  x <- (many $ satisfy (/= '<'))
  char '<'
  t <- many parseType
  char '>'
  return ""

parseSingleTypeVariable :: ReadP [Char]
parseSingleTypeVariable = do
  char 'T'
  typeVariable <- get
  return [typeVariable]


--Ljava/util/Map<TX;+TY;>;
--Ljava/lang/String;
parseReferenceType :: ReadP [Char]
parseReferenceType = parseSimpleRefType
                 <|> parseGenericRefType
                 <|> parseSingleTypeVariable

parseVoid :: ReadP [Char]
parseVoid = do
  return "VOID"

parseReturnType :: ReadP [Char]
parseReturnType = do
  firstChar <- get
  case firstChar of
    'L' -> parseReferenceType
    'V' -> parseVoid
    'I' -> return "Integer"
    'B' -> return "Byte"

-- loop = p <|> (get >> loop)
getAll :: ReadP a -> ReadP [a]
getAll p = many loop
  where
    loop = p <++ (get >> loop)

showText :: Show a => a -> Text
showText = T.pack . show
 -- <E:Ljava/lang/Object;>Ljava/lang/Object;Ljava/lang/Iterable<TE;>;
 -- <K:Ljava/lang/Object;V:Ljava/lang/Object;>Ljava/lang/Object;
 -- Ljava/lang/Object;
splitClassSignature :: ReadP [Char]
splitClassSignature = do
  char '<'
  x <- (many $ satisfy (/= '>'))
  char '>'
  return x

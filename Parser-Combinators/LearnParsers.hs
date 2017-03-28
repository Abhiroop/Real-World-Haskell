module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' >> eof >> stop

one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' >> eof >> stop

oneTwo' =  oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

---------------------------------------------------------------
oneStr :: Parser String
oneStr = string "1"

oneTwoStr :: Parser String
oneTwoStr = string "12"

oneTwoThreeStr :: Parser String
oneTwoThreeStr = string "123"

testParse' :: Parser String -> IO ()
testParse' p = print $ parseString p mempty "123"
-------------------------------------------------------------

stringChar :: [Char] -> Parser b
stringChar (x:xs) = char x >> stringChar xs
stringChar [] = eof >> stop

oneTwoThreeChar' = stringChar "123"

testParse'' :: Show a => Parser a -> IO ()
testParse'' p =
  print $ parseString p mempty "123"

module MyParser where

import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))

type FractionOrDecimal = Either Rational Integer

parseFraction :: Parser Rational
parseFraction = do
  numerator   <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator) 


parseIt :: Parser FractionOrDecimal
parseIt = (Left <$> try parseFraction) <|> (Right <$> try decimal)


main = do
  print $ parseString parseIt mempty "123"
  print $ parseString parseIt mempty "1/3"

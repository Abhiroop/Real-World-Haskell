{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
module RecusrsionSchemes2 where

import Control.Arrow
import Data.Monoid
import Data.Function
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

type Algebra f a = f a -> a

newtype Term f = In { out :: f (Term f) }

ten, add, call :: Term Expr
ten  = In (Literal { intVal = 10 })
add  = In (Ident { name = "add" })
call = In (Call { func = add, args = [ten, ten]}) -- add(10, 10)

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

ana :: (Functor f) => CoAlgebra f a -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

countNodes :: Algebra Expr Int
countNodes (Literal _) = 1
countNodes (Ident   _) = 1
countNodes (Unary _ arg)         = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args)        = fn + sum args + 1
countNodes (Index it idx)        = it + idx + 1
countNodes (Paren arg)           = arg + 1

prettyPrint :: Algebra Expr Doc
prettyPrint (Literal i) = P.int i
prettyPrint (Ident s) = P.text s
prettyPrint (Call f as)     = f <> P.parens (P.hcat (P.punctuate "," as))   -- f(a,b...)
prettyPrint (Index it idx)  = it <> P.brackets idx                 -- a[b]
prettyPrint (Unary op it)   = (P.text op) <> it                    -- op x
prettyPrint (Binary l op r) = l <> (P.text op) <> r                -- lhs op rhs
prettyPrint (Paren exp)     = P.parens exp                         -- (op)

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp f = cata (In >>> f)

type CoAlgebra f a = a -> f a

type RAlgebra f a = f (Term f, a) -> a

para :: (Functor f) => RAlgebra f a -> Term f -> a
para f = out >>> fmap (id &&& para f) >>> f

type RAlgebra' f a = Term f -> f a -> a

-- & is reverse function application
para'' :: Functor f => RAlgebra' f a -> Term f -> a
para'' alg t = out t & fmap (para'' alg) & alg t

cata' :: (Functor f) => Algebra f a -> Term f -> a
cata' f = para'' $ const f

type RCoalgebra f a = a -> f (Either (Term f) a)

apo :: (Functor f) => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap fanin <<< f where fanin = either id (apo f)

apo' :: Functor f => RCoalgebra f a -> a -> Term f
apo' f = In <<< fmap (id ||| apo' f) <<< f

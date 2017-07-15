{-# LANGUAGE DeriveFunctor #-}
module RecursionSchemes where

import Control.Arrow

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data Expr a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq, Functor)

data Stmt a
  = Break
  | Continue
  | Empty
  | IfElse (Expr a) [Stmt a] [Stmt a]
  | Return (Maybe (Expr a))
  | While (Expr a) [Stmt a]
  | Expression (Expr a)
  deriving (Show, Eq)

data Term f = In (f (Term f))

out :: Term f -> f (Term f)
out (In t) = t

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a

bottomUp f = out >>> fmap (bottomUp f) >>> In >>> f

topDown f =  In <<< fmap (topDown f) <<< out <<< f

flattenTerm :: Term Expr -> Term Expr
flattenTerm (In (Paren e)) = e
flattenTerm other = other

flatten :: Term Expr -> Term Expr
flatten = bottomUp flattenTerm

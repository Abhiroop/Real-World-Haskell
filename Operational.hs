{-# LANGUAGE GADTs #-}
module Operational where

import Prelude hiding ((>>=))

data StackInstruction a where
  Push :: Int -> StackInstruction ()
  Pop  :: StackInstruction Int


data Program instr a where
  Then :: instr a -> (a -> Program instr b) -> Program instr b
  Return :: a -> Program instr a

type StackProgram a = Program StackInstruction a

type Stack a = [a]

interpret :: StackProgram a -> (Stack Int -> a)
interpret (Push a `Then` is) stack     = interpret (is ()) (a:stack)
interpret (Pop    `Then` is) (b:stack) = interpret (is b) stack
interpret (Return c) stack             = c

example= Pop `Then` \a -> Pop `Then` \b -> Return (a*b)

singleton :: instr a -> Program instr a
singleton i = i `Then` Return

pop :: StackProgram Int
pop = singleton Pop

push :: Int -> StackProgram ()
push = singleton . Push

(>>=) :: Program i a -> (a -> Program i b) -> Program i b
(Return a)    >>= js = js a
(i `Then` is) >>= js = i `Then` (\a -> is a >>= js)

return :: a -> Program instr a
return = Return

module MapReduce where

import Control.Parallel
import Control.Parallel.Strategies

mapReduce :: Strategy b -> (a -> b) -> Strategy c -> ([b] -> c) -> [a] -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
  mapResult `pseq` reduceResult
  where mapResult = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat

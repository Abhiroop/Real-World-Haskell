import Control.Monad
import Control.Monad.List

type Lio = ListT IO

test3 = getLine >>=
        (\x ->
            (getLine >>=
              putStrLn >>
                 return x)
            >>= putStrLn
        )

test4 =
  do
    x <- [1,2]
    y <- ["a","b"]
    show (x,y)

test5 = ((return test4)::IO String) >>= putStrLn



hello = do
  lift $ putStrLn "This is wrong"

test6 = runListT hello

test :: ListT IO ()
test = do
  x <- return [1,2]
  y <- ListT $ return ['a','b']
  lift $ putStrLn (show (x,y))

test7 = runListT (test >> test)


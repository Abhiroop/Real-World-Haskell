import Control.Monad.State

state_obj :: State [Char] [Char]
state_obj = state (\s -> ("one","two"))

show_result :: Show a => State [Char] a -> IO ()
show_result so =
  let (x,y) = runState so "anything"
  in putStrLn ("First: "  ++ (show $ x)) >>
     putStrLn ("Second: " ++ (show $ y))

test = (show_result (state_obj >> get)) >>
       (show_result (return "interior" >>= put))

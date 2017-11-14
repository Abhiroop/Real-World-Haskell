module MoreIndexed where

infixl 1 ?>>=
class IxMonad m where
  ipure :: a -> m x x a
  (?>>=) :: m x y a -> (a -> m y z b) -> m x z b

data Ready
data EmptyPlate
data BottomBunOn
data PattyOn
data CheeseOn
data OnionOn
data LettuceOn
data TomatoOn
data PicklesOn
data TopBunOn

newtype Ingredient = Ingredient String deriving Show

type BurgerSpec = [Ingredient]

newtype IxBurgerBuilder i o spec = IxBurgerBuilder spec deriving Show

runIxBurgerBuilder :: IxBurgerBuilder prev next spec -> spec
runIxBurgerBuilder (IxBurgerBuilder spec) = spec

instance IxMonad IxBurgerBuilder where
  ipure = IxBurgerBuilder
  (IxBurgerBuilder spec) ?>>= f = IxBurgerBuilder $ runIxBurgerBuilder $ f spec

getEmptyPlate :: IxBurgerBuilder Ready EmptyPlate BurgerSpec
getEmptyPlate = IxBurgerBuilder mempty

addIngredient :: String -> BurgerSpec -> IxBurgerBuilder i o BurgerSpec
addIngredient x xs = IxBurgerBuilder $ Ingredient x : xs

placeEmptyBun :: BurgerSpec -> IxBurgerBuilder EmptyPlate BottomBunOn BurgerSpec
placeEmptyBun = addIngredient "Bottom Bun"

addKetchup :: BurgerSpec -> IxBurgerBuilder BottomBunOn BottomBunOn BurgerSpec
addKetchup = addIngredient "Ketchup"

addPatty :: BurgerSpec -> IxBurgerBuilder BottomBunOn PattyOn BurgerSpec
addPatty = addIngredient "Patty"

addCheese :: BurgerSpec -> IxBurgerBuilder PattyOn CheeseOn BurgerSpec
addCheese = addIngredient "Cheese"

noCheese :: BurgerSpec -> IxBurgerBuilder PattyOn CheeseOn BurgerSpec
noCheese = IxBurgerBuilder

addOnions :: BurgerSpec -> IxBurgerBuilder CheeseOn OnionOn BurgerSpec
addOnions = addIngredient "Onion"

noLettuce :: BurgerSpec -> IxBurgerBuilder OnionOn LettuceOn BurgerSpec
noLettuce = IxBurgerBuilder

addTomato :: BurgerSpec -> IxBurgerBuilder LettuceOn TomatoOn BurgerSpec
addTomato = addIngredient "Tomato"

addTopBun :: BurgerSpec -> IxBurgerBuilder TomatoOn TopBunOn BurgerSpec
addTopBun = addIngredient "TopBun"

burgerSpec :: IxBurgerBuilder Ready TopBunOn BurgerSpec
burgerSpec = getEmptyPlate
  ?>>= placeEmptyBun
  ?>>= addKetchup
  ?>>= addPatty
  ?>>= addCheese
  ?>>= addOnions
  ?>>= noLettuce
  ?>>= addTomato
  ?>>= addTopBun

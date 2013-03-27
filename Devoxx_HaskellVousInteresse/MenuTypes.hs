module MenuTypes where

import qualified Data.Map as M

type Name = String
type Money = Int

totalMoney :: Money
totalMoney = 1505

data MenuItem = MenuItem { itemName :: Name,
                           itemPrice :: Money }
    deriving (Show, Eq, Ord)

newtype Menu = Menu { menuItems :: [MenuItem] }
    deriving (Show)

menu :: Menu
menu = Menu [MenuItem "Mixed Fruit" 215,       MenuItem "French Fries" 275,
             MenuItem "Side Salad" 335,        MenuItem "Hot Wings" 355,
             MenuItem "Mozzarella Sticks" 420, MenuItem "Sampler Plate" 580]


newtype Selection = Selection { selectionItems :: M.Map MenuItem Int }
    deriving (Show, Eq)

-- We introduce some helpers to work with Selections (they will
-- enhance the readability of the code of the solutions, and avoid the
-- fact that everything is coming at once):

emptySelection = Selection (M.empty)

-- | Little intro to pattern matching
addToSelection newItem (Selection items) =
    Selection (M.insertWith (+) newItem 1 items)

-- | Shows the fact that fields names can be used as functions
-- And introduces lazy evaluation: M.toList does not _copy_ the whole sel into
-- a new list
selectionCost sel =
    sum ( map (\(item, quantity) -> itemPrice item * quantity)
              (M.toList (selectionItems sel)) )

-- No types given, so at this point we can show that the interpreter can
-- infer the right types for addToSelection and selectionCost
-- Talk a bit about the unusual -> in type signatures.


-- We just show an equivalent with list comprehensions (so we can use list
-- comprehensions in the first solution):
selectionCost_Ver2 sel =
    sum [ itemPrice item * quantity
          | (item, quantity) <- M.toList (selectionItems sel) ]


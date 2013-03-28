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


emptySelection = Selection (M.empty)

addToSelection newItem (Selection items) =
    Selection (M.insertWith (+) newItem 1 items)

selectionCost sel =
    sum ( map (\(item, quantity) -> itemPrice item * quantity)
              (M.toList (selectionItems sel)) )



selectionCost_Ver2 sel =
    sum [ itemPrice item * quantity
          | (item, quantity) <- M.toList (selectionItems sel) ]


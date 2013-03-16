import Control.Monad


type Name = String
type Price = Int

data MenuItem = MenuItem { itemName :: Name,
                           itemPrice :: Price }
    deriving (Show)

newtype Menu = Menu { menuItems :: [MenuItem] }

menu :: Menu
menu = Menu [MenuItem "Mixed Fruit" 215, MenuItem "French Fries" 275,
             MenuItem "Side Salad" 335, MenuItem "Hot Wings" 355,
             MenuItem "Mozzarella Sticks" 420, MenuItem "Sampler Plate" 580]

totalMoney :: Price
totalMoney = 1505

newtype Selection = Selection { selectionItems :: [MenuItem] }
    deriving (Show)


firstSolution :: Menu -> (Price -> Bool) -> Selection
firstSolution menu pred =
    head [ y | 
            i <- [0..],
            y <- allSelectionsOfSize i,
            pred (sum (map itemPrice (selectionItems y))) ]
    where
        allSelectionsOfSize 0 = [Selection []]
        allSelectionsOfSize n =
            [ Selection (i : selectionItems sel) |
                i <- menuItems menu,
                sel <- allSelectionsOfSize (n-1) ]




{-
secondSolution menu cost =
    head (filter (\sol -> sum (map itemPrice sol) == cost)
                 (concat (map allSelectionsOfSize [0..])) )
    where
        allSelectionsOfSize 0 = [[]]
        allSelectionsOfSize n = concat (map (\item -> map (item :)
                                                          (allSelectionsOfSize (n-1)))
                                            menu)


thirdSolution menu cost = 
    head [ y |
            i <- [0..],
            y <- replicateM i menu,
            sum (map snd y) == cost]
-}
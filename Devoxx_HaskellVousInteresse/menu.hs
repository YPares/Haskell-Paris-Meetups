import Control.Monad


totalMoney :: Int
totalMoney = 1505

menu :: [(String, Int)]
menu = [("Mixed Fruit", 215),       ("French Fries", 275),
        ("Side Salad", 335),        ("Hot Wings", 355),
        ("Mozzarella Sticks", 420), ("Sampler Plate", 580)]

hardcoreSolution menu money = 
    head [ y |
            i <- [0..],
            y <- replicateM i menu,
            sum (map snd y) == money]


-- Now, the same, but with some new types:

type Name = String
type Money = Int

data MenuItem = MenuItem { itemName :: Name,
                           itemPrice :: Money }
    deriving (Show)

newtype Menu = Menu { menuItems :: [MenuItem] }
    deriving (Show)

menuWithNewTypes :: Menu
menuWithNewTypes = Menu [MenuItem "Mixed Fruit" 215,       MenuItem "French Fries" 275,
                         MenuItem "Side Salad" 335,        MenuItem "Hot Wings" 355,
                         MenuItem "Mozzarella Sticks" 420, MenuItem "Sampler Plate" 580]

newtype Selection = Selection { selectionItems :: [MenuItem] }
    deriving (Show)

-- Fist we introduce some helpers to work with Selections (they will
-- enhance the readability of the code that comes afterwards, and avoid the
-- fact that everything is coming at once):

-- | Little intro to pattern matching
addToSelection newItem (Selection items) = Selection (newItem:items)

-- | Shows the fact that fields names can be used as functions
-- and that functions are composable with .
selectionCost sel = sum (map itemPrice
                             (selectionItems sel))

-- No types given, so at this point we can show that the interpreter can
-- infer the right types for addToSelection and selectionCost

-- Then come the solutions:

-- | Regular map/filter/concat solution. No special syntax (i.e. no list
-- comprehensions)
firstSolution :: Menu -> Money -> Selection
firstSolution menu money =
    head ( filter (\sel -> selectionCost sel == money)
                  (concat (map allSelectionsOfSize [0..]))   )
    where
        allSelectionsOfSize :: Int -> [Selection]
        allSelectionsOfSize 0 = [Selection []]
        allSelectionsOfSize n =
            concat ( map (\item -> map (addToSelection item)
                                       (allSelectionsOfSize (n-1)))
                         (menuItems menu) )


-- | The same, but using some list comprehensions to enhance readability,
-- as we make heavy use of map, filter and concat *at the same time*
secondSolution :: Menu -> Money -> Selection
secondSolution menu money =
    head [ sel |
            n   <- [0..],
            sel <- allSelectionsOfSize n,
            selectionCost sel == money ]
    where
        allSelectionsOfSize :: Int -> [Selection]
        allSelectionsOfSize 0 = [Selection []]
        allSelectionsOfSize n =
            [ addToSelection item sel |
                item <- menuItems menu,
                sel  <- allSelectionsOfSize (n-1) ]


-- | The same, but with a predicate parameter, instead of
-- a fixed account:
thirdSolution :: Menu -> (Money -> Bool) -> Selection
thirdSolution menu predicate =
    head [ sel |
            n   <- [0..],
            sel <- allSelectionsOfSize n,
            predicate (selectionCost sel) ]  -- the only line that changes
    where
        allSelectionsOfSize :: Int -> [Selection]
        allSelectionsOfSize 0 = [Selection []]
        allSelectionsOfSize n =
            [ addToSelection item sel |
                item <- menuItems menu,
                sel  <- allSelectionsOfSize (n-1) ]


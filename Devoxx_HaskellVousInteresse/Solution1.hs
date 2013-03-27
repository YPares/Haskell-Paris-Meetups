module Solution1 where

import MenuTypes

-- | Shows inner functions, boolean clauses in list comprehensions
-- (translated to a call to filter) and infinite lists.
solution1 :: Menu -> Money -> Selection
solution1 menu money =
    head [ sel |
            n   <- [0..],
            sel <- allSelectionsOfSize n,
            selectionCost sel == money ]
    where
        allSelectionsOfSize :: Int -> [Selection]
        allSelectionsOfSize 0 = [emptySelection]
        allSelectionsOfSize n =
            [ addToSelection item sel |
                item <- menuItems menu,
                sel  <- allSelectionsOfSize (n-1) ]


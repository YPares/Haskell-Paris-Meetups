module Solution3 where

import MenuTypes

solution3 :: Menu -> (Money -> Bool) -> Selection
solution3 menu predicate =
    head [ sel |
            n   <- [0..],
            sel <- allSelectionsOfSize n,
            predicate (selectionCost sel) ]
    where
        allSelectionsOfSize :: Int -> [Selection]
        allSelectionsOfSize 0 = [emptySelection]
        allSelectionsOfSize n =
            [ addToSelection item sel |
                item <- menuItems menu,
                sel  <- allSelectionsOfSize (n-1) ]


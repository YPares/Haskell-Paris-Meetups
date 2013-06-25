module Solution1 where

import MenuTypes

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


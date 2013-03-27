module Solution3 where

import MenuTypes

-- | If we have time, the same algorithm than solution1,
-- but with a predicate parameter, instead of a fixed account.
-- Used to show that few modifications are needed to get to a more generic
-- code.
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


module Solution2 where

import MenuTypes

solution2 :: Menu -> Money -> Selection
solution2 menu money =
    head ( filter (\sel -> selectionCost sel == money)
                  (concat (map allSelectionsOfSize [0..]))   )
    where
        allSelectionsOfSize :: Int -> [Selection]
        allSelectionsOfSize 0 = [emptySelection]
        allSelectionsOfSize n =
            concat ( map (\item -> map (addToSelection item)
                                       (allSelectionsOfSize (n-1)))
                         (menuItems menu) )


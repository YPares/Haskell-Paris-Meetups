module Solution2 where

import MenuTypes

-- | Shows the desugared, regular map/filter/concat solution.
-- Intended to show that list comprehensions are a thin layer over pure
-- Haskell functions, i.e. that they enhance the readability but not
-- the size of the code. So, they are not fundamental.
--
-- We shouldn't spend more than 30 seconds on that code, the idea is just
-- to show sytactic-sugar-free code, and *insist* on the fact that *it is
-- exactly the same algorithm that is performed!*
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


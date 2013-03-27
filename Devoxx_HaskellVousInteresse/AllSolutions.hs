module AllSolutions where

-- Helper module so than we can :l AllSolution.hs in GHCi and get all the
-- functions available
-- (GHCi erases the current namespace when you :l a file, so we have to
-- load all the modules at once)
--
-- We'll need it to compare the results of the different solutions, and
-- show they're the same

import MenuTypes
import Solution1
import Solution2
import Solution3


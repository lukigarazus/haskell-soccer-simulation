module PlannedMatch where

import Team
import Lib
import Data.List

data PlannedMatch = PlannedMatch { home' :: Team, away' :: Team }

getTeamsFromPlannedMatch pm = [home' pm, away' pm]

instance Show PlannedMatch where
  show PlannedMatch { home' = h, away' = a } = name h ++ " vs " ++ name a
instance Eq PlannedMatch where
  PlannedMatch { home' = h, away' = a } == PlannedMatch { home' = h', away' = a' } = (a == a' && h == h') || (a == h' && h == a')
-- instance Ord PlannedMatch where
--   compare PlannedMatch { home' = h, away' = a } PlannedMatch { home' = h', away' = a' }
--     | a == a' && h == h' = EQ
--     | a == h' && a' == h = GT
--     | otherwise          = LT
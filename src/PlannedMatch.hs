module PlannedMatch where

import Team
import Lib

data PlannedMatch = PlannedMatch { home' :: Team, away' :: Team }

getPlannedMatches :: [Team] -> [PlannedMatch]
getPlannedMatches teams = map (\(h,a) -> PlannedMatch h a) (uniqPairs teams)

instance Show PlannedMatch where
  show PlannedMatch { home' = h, away' = a } = name h ++ " vs " ++ name a
instance Eq PlannedMatch where
  PlannedMatch { home' = h, away' = a } == PlannedMatch { home' = h', away' = a' } = (a == a' && h == h')
-- instance Ord PlannedMatch where
--   compare PlannedMatch { home' = h, away' = a } PlannedMatch { home' = h', away' = a' }
--     | a == a' && h == h' = EQ
--     | a == h' && a' == h = GT
--     | otherwise          = LT
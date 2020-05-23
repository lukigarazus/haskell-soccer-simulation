module Match where

import Team
import PlannedMatch
import Lib

data Match = Match { home :: Team, away :: Team, homeScore :: Int, awayScore :: Int } deriving Show

playMatch :: [Int] -> PlannedMatch -> (Match, [Int])
playMatch randomGoals PlannedMatch { home' = h, away' = a } = 
    (match, restOfRandomGoals)
    where match             = Match h a (head randomGoals) (head . tail $ randomGoals)
          restOfRandomGoals = safeTail . safeTail $ randomGoals -- this safeTail should not be needed
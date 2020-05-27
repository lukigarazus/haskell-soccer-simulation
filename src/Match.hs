module Match where

import Team
import PlannedMatch
import Lib
import Data.List

data Match = Match { home :: Team, away :: Team, homeScore :: Int, awayScore :: Int }

playMatch :: [Int] -> PlannedMatch -> (Match, [Int])
playMatch randomGoals PlannedMatch { home' = h, away' = a } = 
    (match, restOfRandomGoals)
    where match             = Match h a (head randomGoals) (head . tail $ randomGoals)
          restOfRandomGoals = safeTail . safeTail $ randomGoals -- this safeTail should not be needed

addMatch :: Team -> Int -> Int -> Team
addMatch Team { points = p, name = n, conceded = c, scored = s } conceded scored
  | conceded > scored = Team n p (c + conceded) (s + scored)
  | conceded < scored = Team n (p + 3) (c + conceded) (s + scored)
  | conceded == scored = Team n (p + 1) (c + conceded) (s + scored)

getNewTeamsFromMatch :: Match -> [Team]
getNewTeamsFromMatch Match { home = h, away = a, homeScore = hs, awayScore = as } = [addMatch h as hs, addMatch a hs as] 

instance Show Match where
  show Match { home = h, away = a, homeScore = hs, awayScore = as } = concat . intersperse " " $ [name h, show hs, "-", show as, name a]
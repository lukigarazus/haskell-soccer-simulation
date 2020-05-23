module Team where

import Data.List

data Team = Team { name :: String, points :: Int, conceded :: Int, scored :: Int }

makeTeam :: String -> Team
makeTeam name = Team name 0 0 0

makeTeams :: [String] -> [Team]
makeTeams names = map makeTeam names

setTeamName :: Team -> String -> Team
setTeamName Team { points = p, conceded = c, scored = s } newName = Team newName p c s

addMatch :: Team -> Int -> Int -> Team
addMatch Team { points = p, name = n, conceded = c, scored = s } conceded scored
  | conceded > scored = Team n p (c + conceded) (s + scored)
  | conceded < scored = Team n (p + 3) (c + conceded) (s + scored)
  | conceded == scored = Team n (p + 1) (c + conceded) (s + scored)

instance Eq Team where
  Team { name = name } == Team { name = name' } = name == name'
instance Show Team where
  show Team { name = a, conceded = b, scored = c, points = d } = concat (intersperse " " (a : (map show [b,c,d]))) 
instance Ord Team where
  compare Team { points = p } Team { points = p' } = compare p p'
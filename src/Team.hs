module Team where

import Data.List
import Lib

data Team = Team { name :: String, points :: Int, conceded :: Int, scored :: Int }

makeTeam :: String -> Team
makeTeam name = Team name 0 0 0

makeTeams :: [String] -> [Team]
makeTeams names = map makeTeam names

setTeamName :: Team -> String -> Team
setTeamName Team { points = p, conceded = c, scored = s } newName = Team newName p c s

mergeTeams :: Team -> Maybe Team -> Team
mergeTeams Team { name = n1, points = p1, conceded = c1, scored = s1 } t2 =
  case t2 of
    Nothing -> Team n1 p1 c1 s1
    Just Team { name = n2, points = p2, conceded = c2, scored = s2 } -> Team n1 (p1 + p2) (c1 + c2) (s1 + s2)

mergeTeamLists :: [Team] -> [Team] -> [Team]
mergeTeamLists tl1 tl2 = map (\t1 -> mergeTeams t1 (find (t1==) tl2) ) tl1

instance Eq Team where
  Team { name = name } == Team { name = name' } = name == name'
instance Show Team where
  show Team { name = a, conceded = b, scored = c, points = d } = concat (intersperse " " (a : (map show [b,c,d]))) 
instance Ord Team where
  compare Team { name = n, points = p, conceded = c, scored = s } Team { name = n', points = p', conceded = c', scored = s' } = 
    case compare p p' of
      EQ -> 
        case compare (s - c) (s' - c') of
          EQ -> compare n' n
          x  -> x
      x  -> x
module Table where

import Data.List
import Team
import Lib

data Table = Table [Team]

unifyName m team
  | length n < m = (setTeamName team) $ n ++ (getWhiteSpaces $ m - length n)
  | otherwise = team
  where n = name team

instance Show Table where
  show (Table ts) = concat (intersperse "\n" (map show (sort ( map (unifyName m) ts))))
    where m = length . name . maximum $ ts
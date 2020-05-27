module Table where

import Data.List
import Team
import Lib

data Table = Table [Team]

unifyName m team
  | length n < m = (setTeamName team) $ n ++ (getWhiteSpaces $ m - length n)
  | otherwise = team
  where n = name team

getWinner ts = name . head . reverse . sort $ ts

instance Show Table where
  show (Table ts) = concat (intersperse "\n" (map show (reverse . sort $ (map (unifyName m) ts))))
    where m = maximum (map (length . name) ts)
module Main where

import Debug.Trace
import Text.JSON.Generic
import System.IO
import Control.Monad
import Season

--------------------------------------




-- table = Table teams
-- matches = getPlannedMatches teams

-- get1 (x, _, _, _, _) = x
-- get2 (_, x, _, _, _) = x
-- get3 (_, _, x, _, _) = x
-- get4 (_, _, _, x, _) = x
-- get5 (_, _, _, _, x) = x

-- play x first = foldl (\acc -> \n -> acc ++ [playSeason (safeHead (drop (n-1) acc) first)] ) [first] . take x . enumFrom $ 1
validateTeamNames tns =
  case tns of
    Error s -> []
    Ok ns   -> ns

main = do
  print "Start"
  fileString <- readFile "team.json"
  let teamNames = validateTeamNames (decode fileString :: Result [String])
  let season = makeSeason "2019/2020" teamNames
  print season
  print "End"
  -- seed <- newStdGen
  -- let randomGoals = take ((length matches) * 2) (randomListIntFromRange (1,10) seed)
  -- let rounds = play 8 (randomGoals, matches, teams, 1, Round [] 0 Nothing)
  -- print . concat . intersperse "\n---------------------------\n" . map show . map Table . map get3 $ rounds
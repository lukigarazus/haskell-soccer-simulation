module Round where

import PlannedMatch
import Match
import Lib
import Team
import Data.List

data Round = Round { roundMatches :: [PlannedMatch], index :: Int, played :: Maybe [Match] }

getTeamsFromRound pm = concat . map (\PlannedMatch { home' = h, away' = a } -> [h,a]) $ pm

findNextMatchForRound pms ro = find (\PlannedMatch { home' = h, away' = a } ->  (not (elem h roundTeams)) && (not (elem a roundTeams)) ) pms
  where roundTeams = traceLog (getTeamsFromRound ro)

makeRound pms i = go pms []
  where go pms ro =
          case findNextMatchForRound pms ro of
            Nothing -> Round ro i Nothing
            Just m  -> go pms (m : ro) 

makeRounds :: [PlannedMatch] -> [Round]
makeRounds pms = go pms []
  where go pms rounds
            | pms == [] = traceLog rounds
            | otherwise = go newPms (newRound : rounds)
              where newRound = makeRound pms ((length rounds) + 1)
                    newPms = removeRoundMatchesFromPlannedMatches pms newRound

accumulatePlayedMatches (ms, randomGoals) pm =
    case playMatch randomGoals pm of
        (m, nRandomGoals) -> (m : ms, nRandomGoals)
        _                 -> (ms, randomGoals)

playRoundMatches randomGoals Round { roundMatches = pms, index = i } = Round pms i $ Just $ fst (foldl accumulatePlayedMatches ([], randomGoals) pms)

getMatchesLength Round { roundMatches = rm } = length rm 

getTeamsFromPlayedRound Round { played = p } =
    case p of
        Nothing -> []
        Just ms -> concat . map (\Match { home = h, away = a, homeScore = hs, awayScore = as } -> [addMatch h as hs, addMatch a hs as]) $ ms

removeRoundMatchesFromPlannedMatches pms Round { roundMatches = ro } =
  filter (not . (flip elem $ ro)) pms

instance Show Round where
    show Round { roundMatches = rm, index = i, played = p } =
        "Round " ++ (show i) ++ ":\n" ++ (case p of
            Nothing -> show rm
            Just m  -> show m)
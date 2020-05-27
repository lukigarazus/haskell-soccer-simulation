module Season where

import Round
import PlannedMatch
import Team
import Match
import Table
import Data.List
import Lib
import Opponents

data Season = Season { teams :: [Team], rounds :: [Round], playedRounds :: [Round], seasonName :: String, table :: Table, currentRound :: Int }

makeSeason :: String -> [String] -> Season
makeSeason name teams = Season teamObjects rounds [] name (Table teamObjects) 1
  where teamObjects    = makeTeams  teams
        firstHalf      = makeOpponents teamObjects
        secondHalf     = reverse $ makeOpponents teamObjects
        firstRounds    = makeRounds firstHalf 0
        secondRounds   = makeRounds secondHalf (length firstRounds)
        rounds         = firstRounds ++ secondRounds

---------------------------------------------------------------------------------------------

accumulatePlayedMatches (matches, randomGoals) pm = (newMatches, newRandomGoals)
  where obj            = playMatch randomGoals pm
        newMatches     = (fst obj) : matches
        newRandomGoals = snd obj

playNextRound Season { teams = ts, rounds = rs, playedRounds = ps, seasonName = sn, table = tb, currentRound = cr } randomGoals = (Season newTeams (tail rs) (ps ++ [newPlayedRound]) sn newTable (cr + 1), newRandomGoals)
  where round = head rs
        playedRoundObj = foldl accumulatePlayedMatches ([], randomGoals) (roundMatches round)
        newPlayedRound = Round (roundMatches round) (index round) (Just (fst playedRoundObj))
        newTeams       = mergeTeamLists ts (concat . map getNewTeamsFromMatch $ (fst playedRoundObj))
        newTable       = Table newTeams
        newRandomGoals = snd playedRoundObj

playXRounds season randomGoals x =
  case x of
    0 -> season
    x -> playXRounds nextSeasonObj nextRandomGoals (x - 1)
      where nextRoundObj    = playNextRound season randomGoals
            nextSeasonObj   = fst nextRoundObj
            nextRandomGoals = snd nextRoundObj

playSeason season randomGoals =
  playXRounds season randomGoals (length $ rounds season)

---------------------------------------------------------------------------------------------

instance Show Season where
  show Season { seasonName = n, table = t, currentRound = i, rounds = r } = concat . intersperse "\n\n" $ ["Season " ++ n, "Played rounds: " ++ (show (i - 1)), "Rounds to play: " ++ (show (length r)),show t]
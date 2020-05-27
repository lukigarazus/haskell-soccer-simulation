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

accumulateMatches (matches, randomGoals) pm = (newMatches, newRandomGoals)
  where obj            = playMatch randomGoals pm
        newMatches     = (fst obj) : matches
        newRandomGoals = snd obj

playRound Season { teams = ts, rounds = rs, playedRounds = ps, seasonName = sn, table = tb, currentRound = cr } randomGoals = Season ts (tail rs) (ps ++ [playedRound]) sn tb cr
  where round = head rs
        playedRound    = foldl accumulateMatches ([], randomGoals) (roundMatches round)
        newPlayedRound = Round (roundMatches round) (index round) (Just (fst playedRound))

instance Show Season where
  show Season { seasonName = n, table = t, currentRound = i, rounds = r } = concat . intersperse "\n\n" $ ["Season " ++ n, "Played rounds: " ++ (show (i - 1)), "Rounds to play: " ++ (show (length r)),show t]
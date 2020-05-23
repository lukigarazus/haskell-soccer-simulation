module Season where

import Round
import PlannedMatch
import Team
import Match
import Table
import Data.List
import Debug.Trace

data Season = Season { teams :: [Team], plannedMatches :: [PlannedMatch], playedMatches :: [Match], rounds :: [Round], seasonName :: String, table :: Table, currentRound :: Int }

makeSeason :: String -> [String] -> Season
makeSeason name teams = Season teamObjects plannedMatches [] rounds name (Table teamObjects) 1
  where teamObjects    = makeTeams  teams
        firstHalf      = getPlannedMatches teamObjects
        secondHalf     = getPlannedMatches $ reverse teamObjects
        plannedMatches = firstHalf ++ secondHalf
        rounds         = makeRounds plannedMatches

-- playSeason (randomGoals, matches, teams, currentRound, round) =
    -- case matches of
        -- [] ->  (randomGoals, matches, teams, currentRound, Round [] 1 Nothing)
        -- ms ->  (nRandomGoals, nMatches, nTeams, (currentRound + 1), round)
        --   where round       = makeRound ms currentRound
        --         playedRoundMatches = playRoundMatches randomGoals round
        --         nTeams = getTeamsFromPlayedRound playedRoundMatches
        --         nMatches = removeRoundMatchesFromPlannedMatches matches round
        --         nRandomGoals = drop ((getMatchesLength round) * 2) randomGoals

instance Show Season where
  show Season { seasonName = n, table = t, currentRound = i, rounds = r } = concat . intersperse "\n\n" $ ["Season " ++ n, "Played rounds: " ++ (show (i - 1)), "Rounds to play: " ++ (show (length r)),show t]
module Round where

import PlannedMatch
import Match
import Lib
import Team
import Data.List
import Opponents

data Round = Round { roundMatches :: [PlannedMatch], index :: Int, played :: Maybe [Match] }

---------------------------------------------------------------

makeRounds opponents modifier = go opponents []
  where go opponents rounds =
          case all (\Opponents { opponents = o } -> (length o) == 0) opponents of
            True  -> rounds
            False -> go newOpponents (rounds ++ (newRound : []))
              where newRound     = makeRound opponents ((length rounds) + 1 + modifier)
                    newOpponents = removeUsedOpponents opponents newRound

findValidOpponent go team potentialOpponents matches opponentsObjects teams =
  case potentialOpponents of
    [] -> Nothing
    (o : rest)  ->
      case elem o teams of
        True  -> findValidOpponent go team rest matches opponentsObjects teams
        False -> 
          case go (tail opponentsObjects) ((PlannedMatch team o) : matches) of
            Nothing -> findValidOpponent go team rest matches opponentsObjects teams
            x       -> x

makeRound' opponentsObjects = go opponentsObjects []
  where go opponentsObjects matches =
          case opponentsObjects of
            []  -> Just matches
            (Opponents { team = t, opponents = o } : rest) ->
              case elem t teams of
                True  -> go rest matches
                False -> findValidOpponent go t o matches opponentsObjects teams
              where teams = concat . map getTeamsFromPlannedMatch $ matches
  
makeRound opponentsObjects i =
  case makeRound' opponentsObjects of
    Nothing -> Round [] (-1) Nothing
    Just x  -> Round x  i    Nothing

areTeamsPlaying h a m =
  case find (\PlannedMatch { home' = h', away' = a' } -> (h' == a || h' == h) && (a' == a || a' == h)) m of
    Nothing -> False
    _       -> True

removeUsedOpponents opponents Round { roundMatches = rm } = map (\Opponents { team = t, opponents = o } -> Opponents t (filter (\o -> not $ areTeamsPlaying t o rm) o)) opponents

---------------------------------------------------------------

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
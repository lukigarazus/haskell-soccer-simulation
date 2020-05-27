module Main where

import Text.JSON.Generic
import System.IO
import Control.Monad
import Season
import Lib
import System.Random
import Table

validateTeamNames tns =
  case tns of
    Error s -> []
    Ok ns   -> ns

func season randomGoals = do
  putStrLn "This is the current table:"
  b <- getLine
  print season
  putStrLn "\n"
  c <- getLine
  putStrLn "What do you want to do?"
  putStrLn "1 - play the whole season\n2 - play next round"
  action <- getLine
  case action of
    "1" -> 
      do
        let finishedSeason = playSeason season randomGoals
        putStrLn ((show finishedSeason) ++ "\n\n\n" ++ "The winner is " ++ (getWinner $ teams finishedSeason))
        d <- getLine
        print "The End. Press enter to exit"
        e <- getLine
        print "Bye"
    "2" ->
      do
        putStrLn "Matches in this round: "
        case rounds season of
          [] -> 
            do
              putStrLn ((show season) ++ "\n\n\n" ++ "The winner is " ++ (getWinner $ teams season))
              d <- getLine
              print "The End. Press enter to exit"
              e <- getLine
              print "Bye"
          rounds ->
            do
              print (head $ rounds)
              t <- getLine
              let nextObj = playNextRound season randomGoals
              putStrLn "Results: "
              print (head . reverse . playedRounds . fst $ nextObj)
              w <- getLine
              func (fst nextObj) (snd nextObj)
    _   -> 
      do 
        putStrLn "No such action"
        func season randomGoals

main = do
  fileString <- readFile "team.json"
  let teamNames = validateTeamNames (decode fileString :: Result [String])
  let season = makeSeason "2019/2020" teamNames
  seed <- newStdGen
  let randomGoals = take ((((length teamNames ) - 1) * 2) * (length teamNames) ) (randomListIntFromRange (1,6) seed)
  putStrLn "Welcome to my soccer simulator. Press Enter to continue"
  a <- getLine
  func season randomGoals
  
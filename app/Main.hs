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

main = do
  fileString <- readFile "team.json"
  let teamNames = validateTeamNames (decode fileString :: Result [String])
  let season = makeSeason "2019/2020" teamNames
  seed <- newStdGen
  let randomGoals = take ((((length teamNames ) - 1) * 2) * (length teamNames) ) (randomListIntFromRange (1,6) seed)
  putStrLn "Welcome to my Premier League simulator. Press Enter to continue"
  a <- getLine
  putStrLn "\n"
  putStrLn "This is the current table:"
  b <- getLine
  print season
  putStrLn "\n"
  c <- getLine
  putStrLn "What do you want to do?"
  putStrLn "1 - play the whole season"
  action <- getLine
  case action of
    "1" -> putStrLn ((show finishedSeason) ++ "\n\n\n" ++ "The winner is " ++ (getWinner $ teams finishedSeason))
      where finishedSeason = playSeason season randomGoals
    _   -> putStrLn "No such action"
  print "The End"
  
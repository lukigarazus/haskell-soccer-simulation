module Opponents where

import Team

data Opponents = Opponents { team :: Team, opponents :: [Team] } deriving Show

makeOpponents :: [Team] -> [Opponents]
makeOpponents teams = map (\team -> Opponents team (filter (\t -> t /= team) teams)) teams
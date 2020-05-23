module Lib where

import Data.List
import Data.Char
import System.Random
import Debug.Trace

uniqPairs l = nub [(x,y) | x <-l, y<-l, x /= y ]

randomListIntFromRange :: (Int, Int) -> StdGen -> [Int]
randomListIntFromRange range seed = map fst . iterate (\(n,s) -> randomR range s) $ ((randomR range seed))

randomList :: Int -> [Int]
randomList seed = randoms (mkStdGen seed) :: [Int]

getWhiteSpaces :: Int -> String
getWhiteSpaces x = concat . take x . repeat $ " "

safeTail l =
    case l of
        [] -> []
        _  -> tail l

safeHead l d =
    case l of 
        [] -> d
        _  -> head l

traceLog x = trace (show x) x
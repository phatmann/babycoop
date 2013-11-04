module Console where

import Calendar
import System.Random
import qualified Data.Map as Map

printSlot :: Slot -> IO ()
printSlot slot = do putStrLn ((show $ person slot) ++ ":" ++ (show $ attendance slot))

printWeek :: Week -> IO ()
printWeek (date@(year, month, day), slots) = do putStrLn ((show month) ++ "/" ++ (show day))
                                                mapM_ printSlot slots
                                                putStrLn ""

printStat :: (Person, Stat) -> IO ()
printStat (person, stat) =  do 
  let inStr   = show $ inCount stat
      outStr  = show $ outCount stat
      hostStr = show $ hostCount stat
  putStrLn $ (show person) ++ ": in=" ++ inStr ++ ", out=" ++ outStr ++ ", host=" ++ hostStr

printStats :: Stats -> IO ()
printStats stats = do
  mapM printStat $ Map.toAscList stats
  putStrLn ""

printDate :: StdGen -> Date -> IO StdGen
printDate randGen date = do
  let (stats, historyCount) = gatherStats date theCalendar
      Just week = lookup date theCalendar
      (updatedWeek, updatedRandGen) = updateWeek randGen stats historyCount (date, week)
  putStrLn $ "Using random seed: " ++ (show randGen)
  putStrLn $ "Stats for " ++ (show historyCount) ++ " weeks:"
  printStats stats
  printWeek updatedWeek
  return updatedRandGen

printDates :: StdGen -> [Date] -> IO ()
printDates randGen [] = return ()
printDates randGen (date:dates) = do
  randGen' <- printDate randGen date
  printDates randGen' dates
  return ()

main :: IO ()
main = do
  randGen <- newStdGen
  printDates randGen [(2013, 11, 18), (2013, 11, 25), (2013, 12, 2), (2013, 12, 9)]
  return ()
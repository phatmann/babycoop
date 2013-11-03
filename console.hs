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
printStats stats = do mapM printStat $ Map.toAscList stats
                      putStrLn ""

main :: IO ()
main = do randGen <- newStdGen
          let date = (2013, 11, 11)
              (stats, historyCount) = gatherStats date theCalendar
              Just week = lookup date theCalendar
              (updatedWeek, _) = updateWeek randGen stats historyCount (date, week)
          printWeek updatedWeek
          putStrLn $ "Stats for " ++ (show historyCount) ++ " weeks:"
          printStats stats
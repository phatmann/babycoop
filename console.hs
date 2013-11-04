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
      lastHosted = show $ lastHostDate stat
  putStrLn $ (show person) ++ ": in=" ++ inStr ++ ", out=" ++ outStr ++ ", lastHosted=" ++ lastHosted

printStats :: Stats -> IO ()
printStats stats = do
  mapM printStat $ Map.toAscList stats
  putStrLn ""

printDate :: StdGen -> History -> Date -> IO (StdGen, Week)
printDate randGen history date = do
  let (stats, week, randGen') = updateWeek randGen history date
  printWeek week
  putStrLn $ "Using random seed: " ++ (show randGen)
  putStrLn $ "Stats for " ++ (show $ length history) ++ " weeks:"
  printStats stats
  return (randGen', week)

printDates :: StdGen -> [Date] -> IO ()
printDates randGen dates@(date:_) = do
  let printDates' :: StdGen -> History -> [Date] -> IO ()
      printDates' randGen history [] = return ()
      printDates' randGen history (date:dates) = do
        (randGen', week) <- printDate randGen history date
        let history' = (drop extra history) ++ [week]
            extra = if length history == personCount then 1 else 0
        printDates' randGen' history' dates
        return ()
      history = gatherHistory date theCalendar
  printDates' randGen history dates

main :: IO ()
main = do
  randGen <- newStdGen
  printDates randGen [(2013, 11, 11), (2013, 11, 18), (2013, 11, 25), (2013, 12, 2), (2013, 12, 9)]
  return ()
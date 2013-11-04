module Console where

import Calendar
import System.Random
import qualified Data.Map as Map

printSlot :: Slot -> IO ()
printSlot slot = do putStrLn $ (show $ person slot) ++ ": " ++ (show $ attendance slot) ++ (showStat $ stat slot)

printWeek :: Week -> IO ()
printWeek (date@(year, month, day), slots) = do putStrLn ((show month) ++ "/" ++ (show day))
                                                mapM_ printSlot slots
                                                
showStat :: Stat -> String
showStat stat =  
  let inStr   = show $ inCount stat
      outStr  = show $ outCount stat
      lastHosted = show $ lastHostDate stat
  in " (in=" ++ inStr ++ ", out=" ++ outStr ++ ", lastHosted=" ++ lastHosted ++ ")"

printDate :: StdGen -> History -> Date -> IO (StdGen, Week)
printDate randGen history date = do
  let (week, randGen') = updateWeek randGen history date
  printWeek week
  putStrLn $ "Using random seed: " ++ (show randGen)
  putStrLn ""
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
  printDates randGen [(2013, 11, 18), (2013, 11, 25), (2013, 12, 2), (2013, 12, 9)]
  return ()
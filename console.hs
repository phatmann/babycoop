module Console where

import Scheduler
import Calendar
import System.Random
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as Map

showDate :: Date -> String
showDate (0, 0, 0) = "never"
showDate (year, month, day) = (show month) ++ "/" ++ (show day) ++ "/" ++ (show year)

printSlot :: Slot -> IO ()
printSlot slot = do putStrLn $ (show $ person slot) ++ ": " ++ (show $ attendance slot) ++ (showStat $ stat slot)

printMeeting :: (Date, Meeting) -> IO ()
printMeeting (_, Meeting date slots) = do putStrLn $ showDate date
                                          mapM_ printSlot slots
                                          putStrLn ""
                                                
showStat :: Stat -> String
showStat stat =  
  let inStr   = show $ inCount stat
      outStr  = show $ outCount stat
      lastHosted = showDate $ lastHostDate stat
  in " (in=" ++ inStr ++ ", out=" ++ outStr ++ ", lastHosted=" ++ lastHosted ++ ")"

main :: IO ()
main = do
  randGen <- newStdGen
  let theCalendar = map (\m -> ((date m), m)) theMeetings
      newCalendar = updateMeetings randGen (2013, 11, 18) 4 theCalendar
  mapM_ printMeeting newCalendar
  mapM_ pp newCalendar
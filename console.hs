module Console where

import Scheduler
import Calendar
import Requests
import System.Random
import Text.Show.Pretty
import qualified Data.Map as Map

showDate :: Date -> String
showDate (0, 0, 0) = "never"
showDate (year, month, day) = (show month) ++ "/" ++ (show day) ++ "/" ++ (show year)

printSlot :: Slot -> IO ()
printSlot slot = do putStrLn $ (show $ person slot) ++ ": " ++ (show $ attendance slot) ++ (showStat $ stat slot)

printMeeting :: Meeting -> IO ()
printMeeting (Meeting d slots) = do putStrLn $ showDate d
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
  let startDate = (2013, 10, 7)
      numWeeks = 6
      fullCalendar = fillInCalendar startDate numWeeks theCalendar
      calendarWithRequests = mergeRequestCalendar fullCalendar theRequests
      newCalendar = updateMeetings startDate numWeeks calendarWithRequests
  mapM_ printMeeting newCalendar
  putStrLn $ ppShow newCalendar
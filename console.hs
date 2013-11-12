module Console where

import Scheduler
import Calendar
import Requests
import Control.Monad.Random
import Text.Show.Pretty
import qualified Data.Map as Map

showDate :: Date -> String
showDate (0, 0, 0) = "not recently"
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
  let startDate = (2013, 11, 11)
      numWeeks = 2
  fullCalendar <- evalRandIO(fillInCalendar startDate numWeeks theCalendar)
  let calendarWithRequests = mergeRequestCalendar fullCalendar theRequests
      newCalendar = updateMeetings startDate numWeeks calendarWithRequests
  mapM_ printMeeting newCalendar
  putStrLn $ ppShow newCalendar
module Console where

import Scheduler
import Requests
import Control.Monad.Random
import Text.Show.Pretty
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy as B (writeFile, readFile)
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
  let inStr      = show $ inCount stat
      outStr     = show $ outCount stat
      absentStr  = show $ absentCount stat
      lastHosted = showDate $ lastHostDate stat
  in " (in=" ++ inStr ++ ", out=" ++ outStr ++ ", absent=" ++ absentStr ++ ", lastHosted=" ++ lastHosted ++ ")"

main :: IO ()
main = do
  calendarJSON <- B.readFile "calendar.json"
  let Just calendar = decode calendarJSON :: Maybe Calendar
  let startDate = (2013, 11, 18)
      numWeeks = 1
  fullCalendar <- evalRandIO(fillInCalendar startDate numWeeks calendar)
  let calendarWithRequests = mergeRequestCalendar fullCalendar theRequests
      newCalendar = updateMeetings startDate numWeeks calendarWithRequests
  mapM_ printMeeting newCalendar
  B.writeFile "updates.json" $ encodePretty newCalendar
  
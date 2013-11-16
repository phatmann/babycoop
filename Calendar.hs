module Calendar where

import Scheduler
import Requests
import Data.ByteString.Lazy as B (writeFile, readFile)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Monad.Random
import System.Directory

instance FromJSON Person
instance FromJSON Attendance
instance FromJSON Status
instance FromJSON Stat
instance FromJSON Slot
instance FromJSON Meeting

instance ToJSON Person
instance ToJSON Attendance
instance ToJSON Status
instance ToJSON Stat
instance ToJSON Slot
instance ToJSON Meeting

calendarFileName = "calendar.json"

updateCalendar :: Date -> Int -> IO ()
updateCalendar startDate numMeetings = do
  calendar <- readCalendar
  calendarExtendedWithNeededDates <- evalRandIO(fillInCalendar startDate numMeetings calendar)
  let calendarWithRequests = mergeRequestCalendar calendarExtendedWithNeededDates theRequests
      updates = updateMeetings startDate numMeetings calendarWithRequests
  -- mapM_ printMeeting updates
  outFileName <- writeCalendar $ applyUpdates calendar updates
  removeFile calendarFileName
  renameFile outFileName calendarFileName
  
readCalendar :: IO Calendar
readCalendar = do
  calendarJSON <- B.readFile calendarFileName
  let Just calendar = decode calendarJSON :: Maybe Calendar
  return calendar

writeCalendar :: Calendar -> IO String
writeCalendar calendar = do
  let tmpFileName = "calendar.json.tmp"
      encodedCalendar = encodePretty calendar
  B.writeFile tmpFileName encodedCalendar
  return tmpFileName


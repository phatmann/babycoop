module Calendar where

import Scheduler
import Data.ByteString.Lazy as B (writeFile, readFile)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Monad.Random
import System.Directory
import Data.Time
import Data.List
 
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

updateCalendar :: Date -> [(Person, Attendance)] -> IO ()
updateCalendar date attendanceUpdates = do
  let numMeetings = personCount * 2
  calendar <- readCalendar
  -- extendedCalendar <- evalRandIO(fillInCalendar date numMeetings calendar)
  let calendarWithRequests = applyAttendanceUpdates calendar date attendanceUpdates
      updates = updateMeetings date numMeetings calendarWithRequests
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

today :: IO Date
today = do
  currentTime <- getCurrentTime 
  let (y,m,d) = toGregorian $ utctDay currentTime
  return (fromIntegral y, m, d)

pastAndFuture :: Calendar -> IO (Calendar, Calendar)
pastAndFuture calendar = do t <- today
                            let inPast (Meeting date _) = date < t 
                            return $ partition inPast calendar


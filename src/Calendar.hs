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

calendarFolder = "data/calendars/"
futureSpan = personCount * 2

maintainCalendar :: String -> IO ()
maintainCalendar calendarFileName = do
  calendar <- readCalendar calendarFileName
  (past, future) <- pastAndFuture calendar
  let calendarWithConfirmations = applyUpdates calendar $ confirmMeetings past
  let lastMeeting = last calendar
      futureShortfall = futureSpan - (length future)
  extendedCalendar <- evalRandIO(fillInCalendar (date lastMeeting) futureShortfall calendarWithConfirmations)
  saveCalendar calendarFileName extendedCalendar

updateCalendar :: String -> Date -> [(Person, Attendance)] -> IO ()
updateCalendar calendarFileName date attendanceUpdates = do
  calendar <- readCalendar calendarFileName
  let updatedCalendar = applyAttendanceUpdates calendar date attendanceUpdates
      updates         = updateMeetings date futureSpan updatedCalendar
  saveCalendar calendarFileName $ applyUpdates calendar updates

saveCalendar :: String -> Calendar -> IO ()
saveCalendar calendarFileName calendar = do
  tmpFileName <- writeCalendarToTempFile calendar
  removeFile calendarFileName
  renameFile tmpFileName calendarFileName

readCalendar :: String -> IO Calendar
readCalendar calendarFileName = do
  calendarJSON <- B.readFile $ calendarFileName
  let Just calendar = decode calendarJSON :: Maybe Calendar
  return calendar

writeCalendarToTempFile :: Calendar -> IO String
writeCalendarToTempFile calendar = do
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

calendarFileNameForUser :: String -> String
calendarFileNameForUser userName = calendarFolder ++ userName ++ ".json"


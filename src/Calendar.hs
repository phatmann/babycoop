module Calendar (
  readCalendar,
  pastAndFuture,
  findMeeting,
  updateCalendar,
  calendarFileNameForUser,
  calendarFolder,
  maintainCalendar,
  Meeting(..),
  Calendar(..),
  Slot(..),
  Status(..),
  Attendance,
  Date
) where

import Scheduler
import Data.ByteString.Lazy as B (writeFile, readFile)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Monad.Random
import System.Directory
import Data.Time
import Data.List
import Data.Maybe
 
instance FromJSON Calendar
instance FromJSON Attendance
instance FromJSON Status
instance FromJSON Stat
instance FromJSON Slot
instance FromJSON Meeting

instance ToJSON Calendar
instance ToJSON Attendance
instance ToJSON Status
instance ToJSON Stat
instance ToJSON Slot
instance ToJSON Meeting

calendarFolder = "data/calendars/"
futureSpan persons = (length persons) * 2

maintainCalendar :: String -> IO ()
maintainCalendar calendarFileName = do
  calendar <- readCalendar calendarFileName
  let ms = meetings calendar
  (past, future) <- pastAndFuture ms
  let calendarWithConfirmations = calendar { meetings = applyUpdates ms $ confirmMeetings past }
      lastMeeting = last ms
      futureShortfall = (futureSpan $ persons calendar) - (length future)
      lastDate = date lastMeeting
  extendedCalendar <- evalRandIO(fillInCalendar lastDate futureShortfall calendarWithConfirmations)
  saveCalendar calendarFileName calendar { meetings = extendedCalendar }
  updateCalendar calendarFileName lastDate []

updateCalendar :: String -> Date -> [(Person, Attendance)] -> IO ()
updateCalendar calendarFileName date attendanceUpdates = do
  calendar <- readCalendar calendarFileName
  let updatedCalendar = calendar { meetings = applyAttendanceUpdates calendar date attendanceUpdates }
      updates         = updateMeetings date (futureSpan $ persons calendar) updatedCalendar
  saveCalendar calendarFileName $ calendar { meetings = applyUpdates (meetings calendar) updates }

saveCalendar :: String -> Calendar -> IO ()
saveCalendar calendarFileName calendar = do
  tmpFileName <- writeCalendarToTempFile calendar
  removeFile calendarFileName
  renameFile tmpFileName calendarFileName

readCalendar :: String -> IO Calendar
readCalendar calendarFileName = do
  calendarJSON <- B.readFile $ calendarFileName
  return $ fromJust (decode calendarJSON :: Maybe Calendar)

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

pastAndFuture :: [Meeting] -> IO ([Meeting], [Meeting])
pastAndFuture calendar = do t <- today
                            let inPast (Meeting date _) = date < t 
                            return $ partition inPast calendar

calendarFileNameForUser :: String -> String
calendarFileNameForUser userName = calendarFolder ++ userName ++ ".json"


module Calendar where

import Scheduler
import Data.ByteString.Lazy as B (writeFile, readFile)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Monad.Random
import System.Directory
import Data.Time
import Data.List
import Data.Maybe
 
instance FromJSON GroupCalendar
instance FromJSON Attendance
instance FromJSON Status
instance FromJSON Stat
instance FromJSON Slot
instance FromJSON Meeting

instance ToJSON GroupCalendar
instance ToJSON Attendance
instance ToJSON Status
instance ToJSON Stat
instance ToJSON Slot
instance ToJSON Meeting

calendarFolder = "data/calendars/"
futureSpan persons = (length persons) * 2

maintainCalendar :: String -> IO ()
maintainCalendar calendarFileName = do
  groupCalendar <- readCalendar calendarFileName
  let cal = calendar groupCalendar
  (past, future) <- pastAndFuture cal
  let calendarWithConfirmations = groupCalendar { calendar = applyUpdates cal $ confirmMeetings past }
      lastMeeting = last cal
      futureShortfall = (futureSpan $ persons groupCalendar) - (length future)
  extendedCalendar <- evalRandIO(fillInCalendar (date lastMeeting) futureShortfall calendarWithConfirmations)
  saveCalendar calendarFileName groupCalendar { calendar = extendedCalendar }

updateCalendar :: String -> Date -> [(Person, Attendance)] -> IO ()
updateCalendar calendarFileName date attendanceUpdates = do
  groupCalendar <- readCalendar calendarFileName
  let updatedCalendar = groupCalendar { calendar = applyAttendanceUpdates groupCalendar date attendanceUpdates }
      updates         = updateMeetings date (futureSpan $ persons groupCalendar) updatedCalendar
  saveCalendar calendarFileName $ groupCalendar { calendar = applyUpdates (calendar groupCalendar) updates }

saveCalendar :: String -> GroupCalendar -> IO ()
saveCalendar calendarFileName groupCalendar = do
  tmpFileName <- writeCalendarToTempFile groupCalendar
  removeFile calendarFileName
  renameFile tmpFileName calendarFileName

readCalendar :: String -> IO GroupCalendar
readCalendar calendarFileName = do
  calendarJSON <- B.readFile $ calendarFileName
  return $ fromJust (decode calendarJSON :: Maybe GroupCalendar)

writeCalendarToTempFile :: GroupCalendar -> IO String
writeCalendarToTempFile groupCalendar = do
  let tmpFileName = "calendar.json.tmp"
      encodedCalendar = encodePretty groupCalendar
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


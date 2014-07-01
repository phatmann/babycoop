{-# LANGUAGE OverloadedStrings #-}

module Calendar (
  readCalendar,
  pastAndFuture,
  findMeeting,
  updateCalendar,
  updateMeetingWeekday,
  calendarFileNameForCoop,
  calendarFolder,
  maintainCalendar,
  saveCalendar,
  emptyStat,
  Meeting(..),
  Calendar(..),
  Slot(..),
  Status(..),
  Stat(..),
  Attendance(..),
  Hosting(..),
  Date,
  Weekday,
  inCount,
  outCount,
  absentCount,
  lastHostDate,
  recentHistoryCount
) where

import Scheduler
import Data.ByteString.Lazy as B (writeFile, readFile)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import System.Directory
import Data.Time
import Data.List
import Data.Maybe
import System.Random
import Test.QuickCheck.Gen
import Control.Applicative

instance FromJSON Calendar
instance FromJSON Attendance
instance FromJSON Hosting
instance FromJSON Status
instance FromJSON Stat
instance FromJSON Meeting

instance FromJSON Slot where
  parseJSON (Object v) =
    Slot    <$> v .:  "person"
            <*> v .:  "attendance"
            <*> v .:? "hosting" .!= CanHost
            <*> v .:  "status"
            <*> v .:  "stat"
            <*> v .:? "recentStat" .!= emptyStat
            <*> v .:  "rank"

instance ToJSON Calendar
instance ToJSON Attendance
instance ToJSON Hosting
instance ToJSON Status
instance ToJSON Stat
instance ToJSON Slot
instance ToJSON Meeting

calendarFolder = "data/calendars/"

maintainCalendar :: String -> IO ()
maintainCalendar calendarFileName = do
  calendar <- readCalendar calendarFileName
  (pastMeetings, futureMeetings) <- pastAndFuture $ meetings calendar
  let confirmedCalendar = confirmPastMeetings calendar pastMeetings
  gen <- getStdGen
  let extendCalendar = extendCalendarIntoFuture (length futureMeetings) confirmedCalendar
      extendedCalendar = unGen extendCalendar gen 99999 
  saveCalendar calendarFileName extendedCalendar

removeMeetingsFromCalendar :: String -> Date -> Int -> IO ()
removeMeetingsFromCalendar calendarFileName date numMeetings = do
  calendar <- readCalendar calendarFileName
  let meetings' = deleteMeetings date numMeetings $ meetings calendar
      calendar' = calendar { meetings = meetings'}
  saveCalendar calendarFileName calendar'

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

pastAndFuture :: [Meeting] -> IO ([Meeting], [Meeting])
pastAndFuture calendar = do
  let today :: IO Date
      today = do
        currentTime <- getCurrentTime 
        let (y,m,d) = toGregorian $ utctDay currentTime
        return (fromIntegral y, m, d)
  t <- today
  let inPast (Meeting date _ _) = date < t 
  return $ partition inPast calendar

calendarFileNameForCoop :: String -> String
calendarFileNameForCoop coopName = calendarFolder ++ coopName ++ ".json"




module Maintain where

import Calendar
import System.Directory

main :: IO ()
main = do
  paths <- getDirectoryContents calendarFolder
  let calendars = map (\p -> calendarFolder ++ p) $ filter (`notElem` [".", "..", ".DS_Store"]) paths
  mapM_ maintainCalendar calendars
  return ()

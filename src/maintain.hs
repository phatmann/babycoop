module Maintain where

import Calendar
import System.Directory
import Control.Monad (forM)

main :: IO ()
main = do
  paths <- getDirectoryContents calendarFolder
  let calendars = map (\p -> calendarFolder ++ p) $ filter (`notElem` [".", ".."]) paths
  mapM_ maintainCalendar calendars
  return ()

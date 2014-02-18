module Console where

import Calendar
import Data.Aeson

addHistoryCount :: String -> IO()
addHistoryCount coopName = do
  calendar <- readCalendar $ calendarFileNameForCoop coopName
  let ms = zip (meetings calendar) [0..]
      updateCount (m, c) = m {historyCount = if c >= 6 then 6 else c}
      meetings' = map updateCount ms
  let calendar' = calendar {meetings = meetings'}
  saveCalendar (calendarFileNameForCoop coopName) calendar'
  
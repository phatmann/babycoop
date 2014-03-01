module Console where

import Calendar
import Data.Aeson

applyFunctionToCalendar :: (Calendar -> Calendar) -> String -> IO ()
applyFunctionToCalendar f coopName = do
  let filename = calendarFileNameForCoop coopName
  calendar <- readCalendar filename
  saveCalendar filename $ f calendar

addHistoryCount :: Calendar -> Calendar
addHistoryCount calendar =
  let ms = zip (meetings calendar) [0..]
      updateCount (m, c) = m {historyCount = if c >= 6 then 6 else c}
  in calendar {meetings = map updateCount ms}

cmd_changeWeekday :: Date -> Weekday -> String -> IO ()
cmd_changeWeekday date weekday coopName = applyFunctionToCalendar (updateMeetingWeekday date weekday) coopName
  
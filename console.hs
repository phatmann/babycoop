module Console where

import Calendar
import Data.Aeson

applyFunctionToCalendar :: (Calendar -> Calendar) -> String -> IO ()
applyFunctionToCalendar f coopName = do
  let filename = calendarFileNameForCoop coopName
  calendar <- readCalendar filename
  saveCalendar filename $ f calendar

apply_addHistoryCount :: Calendar -> Calendar
addHistoryCount calendar =
  let ms = zip (meetings calendar) [0..]
      updateCount (m, c) = m {historyCount = if c >= 6 then 6 else c}
  in calendar {meetings = map updateCount ms}

apply_setIsHostingField :: Calendar -> Calendar
apply_setIsHostingField calendar = 
  let updateMeeting meeting = meeting {slots = map updateSlot $ slots meeting }
      updateSlot slot = if attendance slot == Host
                        then slot {attendance = In, isHosting = True}
                        else slot {isHosting = False}
  in calendar {meetings = map updateMeeting $ meetings calendar}

cmd_setIsHostingField :: String
cmd_setIsHostingField coopName = applyFunctionToCalendar apply_setIsHostingField coopName

cmd_changeWeekday :: Date -> Weekday -> String -> IO ()
cmd_changeWeekday date weekday coopName = applyFunctionToCalendar (updateMeetingWeekday date weekday) coopName
  
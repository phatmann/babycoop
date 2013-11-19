module Console where

import Calendar

main :: IO ()
main = updateCalendar (2013, 12, 17) []
  
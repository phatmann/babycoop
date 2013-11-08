module Calendar where

import Scheduler

theCalendar :: [Week]
theCalendar =
  [((2013, 10, 07),  [slot Rebecca Out Confirmed
                     ,slot Kasey Absent Confirmed
                     ,slot Neha Absent Confirmed
                     ,slot Kate In Confirmed
                     ,slot Erica In Confirmed
                     ,slot Jenny Host Confirmed])
  ,((2013, 10, 14),  [slot Rebecca Host Confirmed
                     ,slot Kasey In Confirmed
                     ,slot Neha In Confirmed
                     ,slot Kate Out Confirmed
                     ,slot Erica Out Confirmed
                     ,slot Jenny Out Confirmed])
  ,((2013, 10, 21),  [slot Rebecca Out Confirmed
                     ,slot Kasey Out Confirmed
                     ,slot Neha Out Confirmed
                     ,slot Kate Host Confirmed
                     ,slot Erica In Confirmed
                     ,slot Jenny In Confirmed])
  ,((2013, 10, 28),  [slot Rebecca In Confirmed
                     ,slot Kasey Out Confirmed
                     ,slot Neha Host Confirmed
                     ,slot Kate In Confirmed
                     ,slot Erica Out Confirmed
                     ,slot Jenny Out Confirmed])
  ,((2013, 11, 4),   [slot Rebecca In Requested
                     ,slot Kasey Host Confirmed
                     ,slot Neha Absent Requested
                     ,slot Kate Out Confirmed
                     ,slot Erica Absent Requested
                     ,slot Jenny Absent Requested])
  ,((2013, 11, 11),  [slot Rebecca Out Confirmed
                     ,slot Kasey Out Requested
                     ,slot Neha Out Confirmed
                     ,slot Kate In Requested
                     ,slot Erica Host Confirmed
                     ,slot Jenny In Confirmed])
  ,((2013, 12, 2),   [slot Jenny Absent Requested])
  ]
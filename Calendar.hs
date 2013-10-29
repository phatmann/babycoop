module Calendar where

data Person = Rebecca | Jenny | Kate | Kasey | Neha | Erica deriving (Show, Eq, Enum, Bounded)
data Attendance = TBD | In | Out | Host | Absent deriving (Show, Eq, Ord)
data Status = Proposed | Confirmed | Requested deriving (Eq, Show)
type Year = Int
type Month = Int
type Day = Int
type Date = (Year, Month, Day)
data Slot = Slot  { person :: Person
                  , attendance :: Attendance
                  , status :: Status
                  } deriving Show
type Week = (Date, [Slot])
type Calendar = [Week]

theCalendar :: Calendar
theCalendar =
  [((2013, 10, 07),  [Slot Rebecca Out Confirmed
                     ,Slot Kasey Absent Confirmed
                     ,Slot Neha Absent Confirmed
                     ,Slot Kate In Confirmed
                     ,Slot Erica In Confirmed
                     ,Slot Jenny Host Confirmed])
  ,((2013, 10, 14),  [Slot Rebecca Host Confirmed
                     ,Slot Kasey In Confirmed
                     ,Slot Neha In Confirmed
                     ,Slot Kate In Confirmed
                     ,Slot Erica Out Confirmed
                     ,Slot Jenny Out Confirmed])
  ,((2013, 10, 21),  [Slot Rebecca Out Confirmed
                     ,Slot Kasey Out Confirmed
                     ,Slot Neha Out Confirmed
                     ,Slot Kate Host Confirmed
                     ,Slot Erica In Confirmed
                     ,Slot Jenny In Confirmed])
  ,((2013, 10, 28),  [Slot Rebecca In Confirmed
                     ,Slot Kasey Out Confirmed
                     ,Slot Neha Host Confirmed
                     ,Slot Kate In Confirmed
                     ,Slot Erica Out Confirmed
                     ,Slot Jenny Out Confirmed])
  ,((2013, 11, 4),   [Slot Rebecca In Requested
                     ,Slot Kasey Host Confirmed
                     ,Slot Neha Absent Requested
                     ,Slot Kate Out Confirmed
                     ,Slot Erica Absent Requested
                     ,Slot Jenny Absent Requested])
  ,((2013, 11, 11),  [Slot Rebecca TBD Proposed
                     ,Slot Kasey TBD Proposed
                     ,Slot Neha TBD Proposed
                     ,Slot Kate TBD Proposed
                     ,Slot Erica Host Proposed
                     ,Slot Jenny TBD Proposed])
  ,((2013, 11, 18),  [Slot Rebecca TBD Proposed
                     ,Slot Kasey TBD Proposed
                     ,Slot Neha TBD Proposed
                     ,Slot Kate Absent Requested
                     ,Slot Erica TBD Proposed
                     ,Slot Jenny Host Proposed])
  ,((2013, 11, 25),   [Slot Rebecca Host Proposed
                     ,Slot Kasey TBD Proposed
                     ,Slot Neha TBD Proposed
                     ,Slot Kate TBD Proposed
                     ,Slot Erica TBD Proposed
                     ,Slot Jenny TBD Proposed])
  ,((2013, 12, 2),   [Slot Rebecca TBD Proposed
                     ,Slot Kasey TBD Proposed
                     ,Slot Neha TBD Proposed
                     ,Slot Kate TBD Proposed
                     ,Slot Erica TBD Proposed
                     ,Slot Jenny Absent Requested])
  ,((2013, 12, 9),   [Slot Rebecca TBD Proposed
                     ,Slot Kasey TBD Proposed
                     ,Slot Neha TBD Proposed
                     ,Slot Kate TBD Proposed
                     ,Slot Erica TBD Proposed
                     ,Slot Jenny TBD Proposed])
  ]
               --((2013, 11, 11), 
               --   [(Rebecca, Out),
               --    (Kasey,   In),
               --    (Neha,    Out),
               --    (Kate,    Out),
               --    (Erica,   Hosting),
               --    (Jenny,   In)]),
               --((2013, 11, 18), 
               --   [(Rebecca, Out),
               --    (Kasey,   In),
               --    (Neha,    In),
               --    (Kate,    Out),
               --    (Erica,   Out),
               --    (Jenny,   Hosting)]),
               ----((2013, 11, 25), 
               ----   [(Rebecca, Hosting),
               ----    (Kasey,   In),
               ----    (Neha,    Out),
               ----    (Kate,    In),
               ----    (Erica,   Out),
               ----    (Jenny,   Out)])
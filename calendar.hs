module Calendar where

-- data Status = Available | Unavailable | Hosting | Requested | Rejected | Out | In 

data Person = Rebecca | Jenny | Kate | Kasey | Neha | Erica deriving (Show, Eq)
data Attendance = TBD | In | Out | Host | Absent deriving (Show, Eq, Ord)
data RequestT = Requested | NotRequested deriving Eq
data LockedT = Locked | Unlocked deriving Eq
type Year = Int
type Month = Int
type Day = Int
type Date = (Year, Month, Day)
data Slot = Slot  { person :: Person
                  , attendance :: Attendance
                  , requested :: RequestT
                  , locked :: LockedT
                  }
type Week = (Date, [Slot])
type Calendar = [Week]

theCalendar :: Calendar
theCalendar =
  [((2013, 10, 07),  [Slot Rebecca Out NotRequested Locked
                     ,Slot Kasey Absent NotRequested Locked
                     ,Slot Neha Absent NotRequested Locked
                     ,Slot Kate In NotRequested Locked
                     ,Slot Erica In NotRequested Locked
                     ,Slot Jenny Host NotRequested Locked])
  ,((2013, 10, 14),  [Slot Rebecca Host NotRequested Locked
                     ,Slot Kasey In NotRequested Locked
                     ,Slot Neha In NotRequested Locked
                     ,Slot Kate In NotRequested Locked
                     ,Slot Erica Out NotRequested Locked
                     ,Slot Jenny Out NotRequested Locked])
  ,((2013, 10, 21),  [Slot Rebecca Out NotRequested Locked
                     ,Slot Kasey Out NotRequested Locked
                     ,Slot Neha Out NotRequested Locked
                     ,Slot Kate Host NotRequested Locked
                     ,Slot Erica In NotRequested Locked
                     ,Slot Jenny In NotRequested Locked])
  ,((2013, 10, 28),  [Slot Rebecca In NotRequested Locked
                     ,Slot Kasey Out NotRequested Locked
                     ,Slot Neha Host NotRequested Locked
                     ,Slot Kate In NotRequested Locked
                     ,Slot Erica Out NotRequested Locked
                     ,Slot Jenny Out NotRequested Locked])
  ,((2013, 11, 4),   [Slot Rebecca TBD NotRequested Unlocked
                     ,Slot Kasey Host NotRequested Locked
                     ,Slot Neha Absent NotRequested Locked
                     ,Slot Kate TBD NotRequested Unlocked
                     ,Slot Erica TBD NotRequested Unlocked
                     ,Slot Jenny Absent NotRequested Locked])
  ,((2013, 11, 11),  [Slot Rebecca TBD NotRequested Unlocked
                     ,Slot Kasey TBD NotRequested Unlocked
                     ,Slot Neha TBD NotRequested Unlocked
                     ,Slot Kate TBD NotRequested Unlocked
                     ,Slot Erica Host NotRequested Locked
                     ,Slot Jenny TBD NotRequested Unlocked])
  ,((2013, 11, 18),  [Slot Rebecca TBD NotRequested Unlocked
                     ,Slot Kasey TBD NotRequested Unlocked
                     ,Slot Neha TBD NotRequested Unlocked
                     ,Slot Kate TBD NotRequested Unlocked
                     ,Slot Erica TBD NotRequested Unlocked
                     ,Slot Jenny Host NotRequested Locked])
  ,((2013, 11, 25),   [Slot Rebecca Host NotRequested Locked
                     ,Slot Kasey TBD NotRequested Unlocked
                     ,Slot Neha TBD NotRequested Unlocked
                     ,Slot Kate TBD NotRequested Unlocked
                     ,Slot Erica TBD NotRequested Unlocked
                     ,Slot Jenny TBD NotRequested Unlocked])
  ,((2013, 12, 2),   [Slot Rebecca TBD NotRequested Unlocked
                     ,Slot Kasey TBD NotRequested Unlocked
                     ,Slot Neha TBD NotRequested Unlocked
                     ,Slot Kate Host NotRequested Locked
                     ,Slot Erica TBD NotRequested Unlocked
                     ,Slot Jenny Absent NotRequested Locked])
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
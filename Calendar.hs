-- {-# OPTIONS -Wall #-}

module Calendar where

import Data.List
import Debug.Trace
import Data.Function (on)
import System.Random
import Shuffle
import Data.Map (Map)
import qualified Data.Map as Map

data Person = Rebecca | Jenny | Kate | Kasey | Neha | Erica deriving (Show, Eq, Enum, Bounded, Ord)
data Attendance = TBD | In | Out | Host | Absent deriving (Show, Eq, Ord)
data Status = Proposed | Confirmed | Requested deriving (Eq, Show)
type Year = Int
type Month = Int
type Day = Int
type Date = (Year, Month, Day)
data Stat = Stat   { inCount :: Int
                   , outCount :: Int
                   , hostCount :: Int
                   } deriving Show
type Stats = Map Person Stat
data Slot = Slot  { person :: Person
                  , attendance :: Attendance
                  , status :: Status
                  } deriving Show
type Week = (Date, [Slot])

personCount :: Int
personCount = (+1) $ fromEnum $ (maxBound :: Person) 

{-
Rules:
# If more than 3 persons are available, remove whomever went out the week prior
# If still more than 3 persons are available, remove whomever went out 2 weeks prior
# If still more than 3 persons are available, randomly choose 3
# Only 2 can reserve a week
# Never babysit more than 2 weeks in a row
# Hosted never chosen
# Rotate who hosts
-}

updateCalendar :: StdGen -> [Week] -> [Week]
updateCalendar randGen calendar = let emptyDate = (0, 0, 0)
                                      emptyWeek = (emptyDate, [])
                                      emptyHistory = take personCount $ repeat emptyWeek
                                   in updateCalendar' emptyHistory randGen calendar

updateCalendar' :: [Week] -> StdGen -> [Week] -> [Week]
updateCalendar' history randGen [] = []
updateCalendar' history randGen (week:remainingWeeks) =
  let (updatedWeek, updatedRandGen) = updateWeek history randGen week
      updatedHistory                = (drop 1 history) ++ [updatedWeek]
  in  updatedWeek : updateCalendar' updatedHistory updatedRandGen remainingWeeks

updateWeek :: [Week] -> StdGen -> Week -> (Week, StdGen)
updateWeek history randGen (date, slots) =
  let (present, absent) = partition isPresent slots
                          where isPresent slot = attendance slot /= Absent

      (available, confirmed) = partition isAvailable present
                               where isAvailable slot = status slot == Proposed

      (shuffledAvailable, updatedRandGen) = shuffle available randGen

      (hosts, guests) = if needHost then (eligibleHosts, ineligibleHosts) else ([], shuffledAvailable)
                        where needHost                         = not $ any isHost confirmed
                              (eligibleHosts, ineligibleHosts) = partitionEligible 1 favoredHosts unfavoredHosts
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost shuffledAvailable
                              isHost slot                      = attendance slot == Host
                              isFavoredToHost slot             = let (_, inCount, hostCount) = inOutHostCount history (person slot)
                                                                 in hostCount == 0 && inCount <= (personCount `div` 2)

      (eligible, notEligible)  = partitionEligible numberEligibleNeeded favored unfavored
                                 where numberEligibleNeeded = max 0 $ (personCount `div` 2) - (length $ filter isIn confirmed)
                                       (favored, unfavored) = partition isFavoredForOut guests
                                       isIn slot            = attendance slot == In || attendance slot == Host
                                       historyCount         = length history
                                       isFavoredForOut slot = let (inCount, outCount, _) = inOutHostCount history (person slot)
                                                              in inCount >= (historyCount `div` 2) || outCount < (historyCount `div` 2)

      newlyOut  = map (\slot -> slot {attendance=Out}) eligible
      newlyIn   = map (\slot -> slot {attendance=In}) notEligible
      newlyHost = map (\slot -> slot {attendance=Host}) hosts

      newSlots    = confirmed ++ absent ++ newlyIn ++ newlyOut ++ newlyHost
      sortedSlots = sortBy (compare `on` attendance) newSlots
  in  ((date, sortedSlots), updatedRandGen)

partitionEligible :: Int -> [Slot] -> [Slot] -> ([Slot], [Slot])
partitionEligible numberEligibleNeeded favored unfavored =
  let (eligibleFavored, ineligibleFavored)      = splitAt numberEligibleNeeded favored
      numberUnfavoredEligibleNeeded             = numberEligibleNeeded - length eligibleFavored
      (eligibleUnfavored, ineligibleUnfavored)  = splitAt numberUnfavoredEligibleNeeded unfavored
  in  (eligibleFavored ++ eligibleUnfavored, ineligibleFavored ++ ineligibleUnfavored)

-- TODO: do one pass through attendance history to gather all stats

gatherStats :: Date -> [Week] -> Stats 
gatherStats date calendar = 
  let dateIndex = case findIndex (\(d, _) -> d == date) calendar of
        Just n -> n
        Nothing -> error "Date not in calendar"
      historyIndex = dateIndex - personCount
      history :: [Week]
      history = take personCount $ if historyIndex <= 0 then calendar else drop historyIndex calendar
      emptyStats = Map.empty :: Stats
      gatherWeekStats :: Stats -> Week -> Stats
      gatherWeekStats stats (_, slots) = foldl incrementSlotStat stats slots
        where incrementSlotStat :: Stats -> Slot -> Stats
              incrementSlotStat stats slot =
                let key = person slot
                    oldStat = Map.findWithDefault (Stat 0 0 0) key stats
                    newStat = case attendance slot of
                      In     -> oldStat {inCount = (inCount oldStat) + 1}
                      Out    -> oldStat {outCount = (outCount oldStat) + 1}
                      Host   -> oldStat {inCount = (inCount oldStat) + 1, hostCount = (hostCount oldStat) + 1}
                      Absent -> oldStat
                      TBD    -> oldStat
                    in Map.alter (\_ -> Just newStat) key stats
      in foldl gatherWeekStats emptyStats history

inOutHostCount :: [Week] -> Person -> (Int, Int, Int)
inOutHostCount history person =
  let attendanceHistory = map (lookupAttendance person) history
      inCount           = length $ filter (\attendance -> attendance `elem` [In, Host]) attendanceHistory
      outCount          = length $ filter (\attendance -> attendance == Out) attendanceHistory
      hostCount         = length $ filter (\attendance -> attendance == Host) attendanceHistory
  in  (inCount, outCount, hostCount)

lookupAttendance :: Person -> Week -> Attendance
lookupAttendance targetPerson week@(date, slots) =
  let result = find (\slot -> (person slot) == targetPerson) slots 
  in case result of
    Nothing   -> Absent
    Just slot -> attendance slot




theCalendar :: [Week]
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
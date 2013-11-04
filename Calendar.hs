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
data Stat = Stat   { inDates :: [Date]
                   , outDates :: [Date]
                   , hostDates :: [Date]
                   } deriving Show
type Stats = Map Person Stat
data Slot = Slot  { person :: Person
                  , attendance :: Attendance
                  , status :: Status
                  } deriving Show
type Week = (Date, [Slot])

personCount :: Int
personCount = (+1) $ fromEnum $ (maxBound :: Person) 

--updateCalendar :: StdGen -> [Week] -> [Week]
--updateCalendar randGen calendar = let emptyDate = (0, 0, 0)
--                                      emptyWeek = (emptyDate, [])
--                                      emptyHistory = take personCount $ repeat emptyWeek
--                                   in updateCalendar' emptyHistory randGen calendar

--updateCalendar' :: [Week] -> StdGen -> [Week] -> [Week]
--updateCalendar' history randGen [] = []
--updateCalendar' history randGen (week:remainingWeeks) =
--  let (updatedWeek, updatedRandGen) = updateWeek history randGen week
--      updatedHistory                = (drop 1 history) ++ [updatedWeek]
--  in  updatedWeek : updateCalendar' updatedHistory updatedRandGen remainingWeeks

-- BUG: should not allow same person to host several times in a row; should rotate

updateWeek :: StdGen -> Stats -> Int -> Week -> (Week, StdGen)
updateWeek randGen stats historyCount  (date, slots) =
  let (present, absent) = partition isPresent slots
                          where isPresent slot = attendance slot /= Absent

      (available, confirmed) = partition isAvailable present
                               where isAvailable slot = status slot == Proposed

      (shuffledAvailable, updatedRandGen) = shuffle available randGen

      personStat slot = findStat (person slot) stats

      (hosts, guests) = if needHost then (eligibleHosts, ineligibleHosts) else ([], shuffledAvailable)
                        where needHost                         = not $ any isHost confirmed
                              (eligibleHosts, ineligibleHosts) = partitionEligible 1 favoredHosts unfavoredHosts
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost shuffledAvailable
                              isHost slot                      = attendance slot == Host
                              isFavoredToHost slot             = let stat = personStat slot
                                                                 in (hostCount stat) == 0 && (inCount stat) <= (personCount `div` 2)

      (eligible, notEligible)  = partitionEligible numberEligibleNeeded favored unfavored
                                 where numberEligibleNeeded = max 0 $ (personCount `div` 2) - (length $ filter isIn confirmed)
                                       (favored, unfavored) = partition isFavoredForOut guests
                                       isIn slot            = attendance slot == In || attendance slot == Host
                                       isFavoredForOut slot = let stat = personStat slot
                                                              in (inCount stat) > (historyCount `div` 2) || (outCount stat) < (historyCount `div` 2)

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

emptyStat :: Stat
emptyStat = Stat [] [] []

findStat :: Person -> Stats -> Stat
findStat = Map.findWithDefault emptyStat

inCount :: Stat -> Int
inCount stat = length $ inDates stat

outCount :: Stat -> Int
outCount stat = length $ outDates stat

hostCount :: Stat -> Int
hostCount stat = length $ hostDates stat

gatherStats :: Date -> [Week] -> (Stats, Int)
gatherStats date calendar = 
  let dateIndex = case findIndex (\(d, _) -> d == date) calendar of
        Just n -> n
        Nothing -> error "Date not in calendar"
      historyIndex = max 0 (dateIndex - personCount)
      historyCount = dateIndex - historyIndex
      history :: [Week]
      history = take historyCount $ drop historyIndex calendar
      emptyStats = Map.empty :: Stats
      gatherWeekStats :: Stats -> Week -> Stats
      gatherWeekStats stats (slotDate, slots) = foldl incrementSlotStat stats slots
        where incrementSlotStat :: Stats -> Slot -> Stats
              incrementSlotStat stats slot =
                let key = person slot
                    oldStat = findStat key stats
                    newStat = case attendance slot of
                      In     -> oldStat {inDates = (inDates oldStat) ++ [slotDate]}
                      Out    -> oldStat {outDates = (outDates oldStat) ++ [slotDate]}
                      Host   -> oldStat {inDates = (inDates oldStat) ++ [slotDate], hostDates = (hostDates oldStat) ++ [slotDate]}
                      Absent -> oldStat
                      TBD    -> oldStat
                    in Map.alter (\_ -> Just newStat) key stats
      stats = foldl gatherWeekStats emptyStats history
      in (stats, historyCount)

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
                     ,Slot Kate Out Confirmed
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
  ,((2013, 11, 11),  [Slot Rebecca Out Confirmed
                     ,Slot Kasey In Confirmed
                     ,Slot Neha Out Confirmed
                     ,Slot Kate Out Confirmed
                     ,Slot Erica Host Confirmed
                     ,Slot Jenny In Confirmed])
  ,((2013, 11, 18),  [Slot Rebecca TBD Proposed
                     ,Slot Kasey TBD Proposed
                     ,Slot Neha TBD Proposed
                     ,Slot Kate Absent Requested
                     ,Slot Erica TBD Proposed
                     ,Slot Jenny TBD Proposed])
  ,((2013, 11, 25),  [Slot Rebecca TBD Proposed
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
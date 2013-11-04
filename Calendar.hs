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
                  , stat :: Stat
                  } deriving Show
type Week = (Date, [Slot])
type History = [Week]

personCount :: Int
personCount = (+1) $ fromEnum $ (maxBound :: Person) 

emptyDate :: Date
emptyDate = (0, 0, 0)

slot :: Person -> Attendance -> Status -> Slot
slot person attendance status = Slot person attendance status emptyStat
      
updateWeek :: StdGen -> History -> Date -> (Week, StdGen)
updateWeek randGen history date =
  let stats = historyStats history
      Just slots = lookup date theCalendar
      (week, randGen') = calcWeek randGen (length history) (date, map statify slots)
      statify slot = slot {stat = findStat (person slot) stats}
  in (week, randGen')

calcWeek :: StdGen -> Int -> Week -> (Week, StdGen)
calcWeek randGen historyCount  (date, slots) =
  let (present, absent) = partition isPresent slots
                          where isPresent slot = attendance slot /= Absent

      (available, confirmed) = partition isAvailable present
                               where isAvailable slot = status slot == Proposed

      (shuffledAvailable, updatedRandGen) = shuffle available randGen

      (hosts, guests) = if needHost then (eligibleHosts, ineligibleHosts) else ([], shuffledAvailable)
                        where needHost                         = not $ any isHost confirmed
                              (eligibleHosts, ineligibleHosts) = choose 1 favoredHosts rankedUnfavoredHosts
                              rankedUnfavoredHosts             = sortBy (compare `on` lastHostDate . stat ) unfavoredHosts
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost shuffledAvailable
                              isHost slot                      = attendance slot == Host
                              isFavoredToHost slot             = let personStat = stat slot
                                                                 in (hostCount personStat) == 0 && (inCount personStat) <= (personCount `div` 2)

      (eligible, notEligible)  = choose numberNeeded favored unfavored
                                 where numberNeeded = max 0 $ (personCount `div` 2) - (length $ filter isIn confirmed)
                                       (favored, unfavored) = partition isFavoredForOut guests
                                       isIn slot            = attendance slot == In || attendance slot == Host
                                       isFavoredForOut slot = let personStat = stat slot
                                                              in (inCount personStat) > (historyCount `div` 2) || (outCount personStat) < (historyCount `div` 2)

      newlyOut  = map (\slot -> slot {attendance=Out}) eligible
      newlyIn   = map (\slot -> slot {attendance=In}) notEligible
      newlyHost = map (\slot -> slot {attendance=Host}) hosts

      newSlots    = confirmed ++ absent ++ newlyIn ++ newlyOut ++ newlyHost
      sortedSlots = sortBy (compare `on` attendance) newSlots
  in  ((date, sortedSlots), updatedRandGen)

choose :: Int -> [Slot] -> [Slot] -> ([Slot], [Slot])
choose numberNeeded favored unfavored =
  let (chosenFavored, rejectedFavored) = splitAt numberNeeded favored
      numberUnfavoredNeeded = numberNeeded - length chosenFavored
      (chosenUnfavored, rejectedUnfavored) = splitAt numberUnfavoredNeeded unfavored
      chosen = chosenFavored ++ chosenUnfavored
      rejected = rejectedFavored ++ rejectedUnfavored
  in  (chosen, rejected)

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

lastHostDate :: Stat -> Date
lastHostDate stat = case hostDates stat of
                      []  -> emptyDate
                      ds  -> last ds

gatherHistory :: Date -> [Week] -> History
gatherHistory date calendar = 
  let Just dateIndex = findIndex (\(d, _) -> d == date) calendar
      historyIndex = max 0 (dateIndex - personCount)
      historyCount = dateIndex - historyIndex
  in take historyCount $ drop historyIndex calendar

historyStats :: History -> Stats
historyStats history = 
  let emptyStats = Map.empty :: Stats
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
      in stats

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
                     ,slot Kasey In Confirmed
                     ,slot Neha Out Confirmed
                     ,slot Kate Out Confirmed
                     ,slot Erica Host Confirmed
                     ,slot Jenny In Confirmed])
  ,((2013, 11, 18),  [slot Rebecca TBD Proposed
                     ,slot Kasey TBD Proposed
                     ,slot Neha TBD Proposed
                     ,slot Kate Absent Requested
                     ,slot Erica TBD Proposed
                     ,slot Jenny TBD Proposed])
  ,((2013, 11, 25),  [slot Rebecca TBD Proposed
                     ,slot Kasey TBD Proposed
                     ,slot Neha TBD Proposed
                     ,slot Kate TBD Proposed
                     ,slot Erica TBD Proposed
                     ,slot Jenny TBD Proposed])
  ,((2013, 12, 2),   [slot Rebecca TBD Proposed
                     ,slot Kasey TBD Proposed
                     ,slot Neha TBD Proposed
                     ,slot Kate TBD Proposed
                     ,slot Erica TBD Proposed
                     ,slot Jenny Absent Requested])
  ,((2013, 12, 9),   [slot Rebecca TBD Proposed
                     ,slot Kasey TBD Proposed
                     ,slot Neha TBD Proposed
                     ,slot Kate TBD Proposed
                     ,slot Erica TBD Proposed
                     ,slot Jenny TBD Proposed])
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
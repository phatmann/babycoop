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
type History = [Week]

personCount :: Int
personCount = (+1) $ fromEnum $ (maxBound :: Person) 

emptyDate :: Date
emptyDate = (0, 0, 0)
      
updateWeek :: StdGen -> History -> Date -> (Stats, Week, StdGen)
updateWeek randGen history date =
  let stats = historyStats history
      Just week = lookup date theCalendar
      (week', randGen') = calcWeek randGen stats (length history) (date, week)
  in (stats, week', randGen')

calcWeek :: StdGen -> Stats -> Int -> Week -> (Week, StdGen)
calcWeek randGen stats historyCount  (date, slots) =
  let (present, absent) = partition isPresent slots
                          where isPresent slot = attendance slot /= Absent

      (available, confirmed) = partition isAvailable present
                               where isAvailable slot = status slot == Proposed

      (shuffledAvailable, updatedRandGen) = shuffle available randGen

      personStat slot = findStat (person slot) stats
      lastSlotHostDate slot = lastHostDate $ personStat slot

      (hosts, guests) = if needHost then (eligibleHosts, ineligibleHosts) else ([], shuffledAvailable)
                        where needHost                         = not $ any isHost confirmed
                              (eligibleHosts, ineligibleHosts) = choose 1 favoredHosts rankedUnfavoredHosts
                              rankedUnfavoredHosts             = sortBy (compare `on` lastSlotHostDate) unfavoredHosts
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost shuffledAvailable
                              isHost slot                      = attendance slot == Host
                              isFavoredToHost slot             = let stat = personStat slot
                                                                 in (hostCount stat) == 0 && (inCount stat) <= (personCount `div` 2)

      (eligible, notEligible)  = choose numberNeeded favored unfavored
                                 where numberNeeded = max 0 $ (personCount `div` 2) - (length $ filter isIn confirmed)
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
  ,((2013, 11, 11),  [Slot Rebecca TBD Proposed
                     ,Slot Kasey TBD Proposed
                     ,Slot Neha TBD Proposed
                     ,Slot Kate TBD Proposed
                     ,Slot Erica TBD Proposed
                     ,Slot Jenny TBD Proposed])
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
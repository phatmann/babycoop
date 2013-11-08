{-# LANGUAGE DeriveGeneric #-}

module Scheduler where

import Data.List
import Debug.Trace
import Data.Function (on)
import System.Random
import Shuffle
import Data.Map (Map)
import Data.Time
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as Map

data Person = Rebecca | Jenny | Kate | Kasey | Neha | Erica deriving (Show, Eq, Enum, Bounded, Ord, Generic)
data Attendance = TBD | In | Out | Host | Absent deriving (Show, Eq, Ord, Generic)
data Status = Proposed | Confirmed | Requested deriving (Eq, Show, Generic)
type Year = Int
type Month = Int
type MDay = Int
type Date = (Year, Month, MDay)
data Stat = Stat   { inDates :: [Date]
                   , outDates :: [Date]
                   , hostDates :: [Date]
                   } deriving (Show, Generic)
type Stats = Map Person Stat
data Slot = Slot  { person :: Person
                  , attendance :: Attendance
                  , status :: Status
                  , stat :: Stat
                  } deriving (Show, Generic)
type Week = (Date, [Slot])
type History = [Week]

instance Out Person
instance Out Attendance
instance Out Status
instance Out Stat
instance Out Slot

personCount :: Int
personCount = (+1) $ fromEnum $ (maxBound :: Person) 

emptyDate :: Date
emptyDate = (0, 0, 0)

slot :: Person -> Attendance -> Status -> Slot
slot person attendance status = Slot person attendance status emptyStat

dateRange :: Date -> Int -> [Date]
dateRange _ 0 = []
dateRange date@(year, month, mday) numWeeks = 
  let numDays = 7 * signum numWeeks 
      (y,m,d) = toGregorian $ addDays (toInteger numDays) $ fromGregorian (toInteger year) month mday
      nextWeek = (fromIntegral y, m, d) :: Date
  in date : dateRange nextWeek (numWeeks - signum numWeeks)

updateWeeks :: StdGen -> Date -> Int -> [Week] -> [Week]
updateWeeks randGen startDate numWeeks calendar =
  let updateWeeks' :: StdGen -> History -> [Date] -> [Week]
      historyBackCount = -(personCount + 1)
      dates@(firstDate:_) = dateRange startDate numWeeks
      backDates = dateRange startDate historyBackCount
      fillerCalendar = map (\d -> (d, [])) $ union backDates dates
      weeksHaveSameDate (date1, _) (date2, _) = date1 == date2
      fullCalendar = sortBy (compare `on` fst) $ unionBy weeksHaveSameDate calendar fillerCalendar
      history = gatherHistory firstDate fullCalendar
      updateWeeks' randGen history [] = []
      updateWeeks' randGen history (d:ds) =
        let (week, randGen') = updateWeek randGen history fullCalendar d
            history' = (drop extra history) ++ [week]
            extra = if length history == personCount then 1 else 0
        in week : updateWeeks' randGen' history' ds
      in updateWeeks' randGen history dates
      
updateWeek :: StdGen -> History -> [Week] -> Date -> (Week, StdGen)
updateWeek randGen history calendar date =
  let stats = historyStats history
      slots = findSlots date calendar
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
                              (eligibleHosts, ineligibleHosts) = choose 1 rankedFavoredHosts rankedUnfavoredHosts
                              rankedFavoredHosts               = sortBy (compare `on` lastHostDate . stat ) favoredHosts
                              rankedUnfavoredHosts             = sortBy (compare `on` lastHostDate . stat ) unfavoredHosts
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost shuffledAvailable
                              isHost slot                      = attendance slot == Host
                              isFavoredToHost slot             = let personStat = stat slot
                                                                 in (inCount personStat) <= (personCount `div` 2)

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

findSlots :: Date -> [Week] -> [Slot]
findSlots date calendar =
  let dateSlots = case lookup date calendar of
        Just ss -> ss
        Nothing -> []
      allPeople = [minBound .. maxBound] :: [Person]
      allPeopleSlots = map (\p -> slot p TBD Proposed) allPeople
      slotsHaveSamePerson slot1 slot2 = (person slot1) == (person slot2)
  in unionBy slotsHaveSamePerson dateSlots allPeopleSlots

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

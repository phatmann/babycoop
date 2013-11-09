{-# LANGUAGE DeriveGeneric #-}

module Scheduler where

import Data.List
import Debug.Trace
import Data.Function (on)
import System.Random
import Shuffle
import Data.Map (Map)
import Data.Time
import qualified Data.Map as Map

data Person = Rebecca | Jenny | Kate | Kasey | Neha | Erica deriving (Show, Eq, Enum, Bounded, Ord)
data Attendance = TBD | In | Out | Host | Absent deriving (Show, Eq, Ord)
data Status = Proposed | Confirmed | Requested deriving (Eq, Show)
type Year = Int
type Month = Int
type MDay = Int
type Date = (Year, Month, MDay)
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
data Meeting = Meeting { date :: Date, slots :: [Slot] } deriving Show
type CalendarEntry = (Date, Meeting)
type Calendar = [CalendarEntry]

personCount :: Int
personCount = (+1) $ fromEnum $ (maxBound :: Person) 

emptyDate :: Date
emptyDate = (0, 0, 0)

slot :: Person -> Attendance -> Status -> Slot
slot person attendance status = Slot person attendance status emptyStat

dateRange :: Date -> Int -> [Date]
dateRange _ 0 = []
dateRange date@(year, month, mday) numMeetings = 
  let numDays = 7 * signum numMeetings 
      (y,m,d) = toGregorian $ addDays (toInteger numDays) $ fromGregorian (toInteger year) month mday
      nextMeeting = (fromIntegral y, m, d) :: Date
  in date : dateRange nextMeeting (numMeetings - signum numMeetings)

mergeCalendars :: Calendar -> Calendar -> Calendar
mergeCalendars calendar1 calendar2 = 
  let meetingsHaveSameDate (date1, _) (date2, _) = date1 == date2
  in sortBy (compare `on` fst) $ unionBy meetingsHaveSameDate calendar1 calendar2

updateMeetings :: StdGen -> Date -> Int -> Calendar -> Calendar
updateMeetings randGen startDate numMeetings calendar =
  let historyBackCount = -(personCount + 1)
      backDates = dateRange startDate historyBackCount
      fillerCalendar = map (\d -> (d, Meeting {date=d, slots=[]})) $ union backDates dates
      fullCalendar = mergeCalendars calendar fillerCalendar

      dates@(firstDate:_) = dateRange startDate numMeetings
      history = gatherHistory firstDate fullCalendar

      updateMeetings' :: StdGen -> Calendar -> [Date] -> Calendar
      updateMeetings' randGen history [] = []
      updateMeetings' randGen history (d:ds) =
        let (meeting, randGen') = updateMeeting randGen history fullCalendar d
            history' = (drop extra history) ++ [(d, meeting)]
            extra = if length history == personCount then 1 else 0
        in (d, meeting) : updateMeetings' randGen' history' ds
      in updateMeetings' randGen history dates
      
updateMeeting :: StdGen -> Calendar -> Calendar -> Date -> (Meeting, StdGen)
updateMeeting randGen history calendar date =
  let stats = historyStats history
      meeting = findMeeting date calendar
      (meeting', randGen') = calcMeeting randGen (length history) $ statifyMeeting meeting
      statifySlot slot = slot {stat = findStat (person slot) stats}
      statifyMeeting m = m {slots = map statifySlot (slots m)}
  in (meeting', randGen')

calcMeeting :: StdGen -> Int -> Meeting -> (Meeting, StdGen)
calcMeeting randGen historyCount  (Meeting date slots) =
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
  in  (Meeting date sortedSlots, updatedRandGen)

choose :: Int -> [Slot] -> [Slot] -> ([Slot], [Slot])
choose numberNeeded favored unfavored =
  let (chosenFavored, rejectedFavored) = splitAt numberNeeded favored
      numberUnfavoredNeeded = numberNeeded - length chosenFavored
      (chosenUnfavored, rejectedUnfavored) = splitAt numberUnfavoredNeeded unfavored
      chosen = chosenFavored ++ chosenUnfavored
      rejected = rejectedFavored ++ rejectedUnfavored
  in  (chosen, rejected)

findMeeting :: Date -> Calendar -> Meeting
findMeeting date calendar =
  let meeting = case lookup date calendar of
        Just m -> m
        Nothing -> Meeting date []
      allPeople = [minBound .. maxBound] :: [Person]
      allPeopleSlots = map (\p -> slot p TBD Proposed) allPeople
      slotsHaveSamePerson slot1 slot2 = (person slot1) == (person slot2)
      mergedSlots = unionBy slotsHaveSamePerson (slots meeting) allPeopleSlots
  in meeting {slots = mergedSlots }

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

gatherHistory :: Date -> Calendar -> Calendar
gatherHistory date calendar = 
  let Just dateIndex = findIndex (\(d, _) -> d == date) calendar
      historyIndex = max 0 (dateIndex - personCount)
      historyCount = dateIndex - historyIndex
  in take historyCount $ drop historyIndex calendar

historyStats :: Calendar -> Stats
historyStats history = 
  let emptyStats = Map.empty :: Stats
      gatherMeetingStats :: Stats -> CalendarEntry -> Stats
      gatherMeetingStats stats (_, Meeting slotDate slots) = foldl incrementSlotStat stats slots
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
      stats = foldl gatherMeetingStats emptyStats history
      in stats

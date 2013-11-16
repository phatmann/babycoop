{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Scheduler where

import Data.List
import Debug.Trace
import Data.Function (on)
import Data.Map (Map)
import Data.Time
import Data.Ratio
import Control.Monad.Random
import GHC.Generics
import qualified Data.Map as Map

data Person = Erica | Jenny | Kasey | Kate | Neha | Rebecca  deriving (Show, Eq, Enum, Bounded, Ord, Generic)
data Attendance = TBD | In | Out | Host | Absent deriving (Show, Eq, Ord, Generic)
data Status = Proposed | Confirmed | Requested deriving (Eq, Show, Generic)
type Year = Int
type Month = Int
type MDay = Int
type Date = (Year, Month, MDay)
type Rank = Int
data Stat = Stat   { inDates :: [Date]
                   , outDates :: [Date]
                   , hostDates :: [Date]
                   , absentDates :: [Date]
                   } deriving (Show, Generic)
type Stats = Map Person Stat
data Slot = Slot  { person :: Person
                  , attendance :: Attendance
                  , status :: Status
                  , stat :: Stat
                  , rank :: Rank
                  } deriving (Show, Generic)
data Meeting = Meeting { date :: Date, slots :: [Slot] } deriving (Show, Generic)
type Calendar = [Meeting]

updateMeetings :: Date -> Int -> Calendar -> Calendar
updateMeetings startDate numMeetings calendar =
  let updateMeetings' :: Calendar -> [Date] -> Calendar
      updateMeetings' history [] = []
      updateMeetings' history (d:ds) =
        let meeting = updateMeeting history calendar d
            history' = (drop extra history) ++ [meeting]
            extra = if length history == personCount then 1 else 0
        in meeting : updateMeetings' history' ds
      initialHistory = gatherHistory startDate calendar
  in updateMeetings' initialHistory $ dateRange startDate numMeetings

deleteMeetings :: Date -> Int -> Calendar -> Calendar
deleteMeetings startDate numMeetings calendar =
  let datesToDelete = dateRange startDate numMeetings
      meetingNotinDatesToDelete meeting = not $ (date meeting) `elem` datesToDelete
  in filter meetingNotinDatesToDelete calendar

-----------------------

personCount :: Int
personCount = (+1) $ fromEnum $ (maxBound :: Person) 

emptyDate :: Date
emptyDate = (0, 0, 0)

slot :: Person -> Attendance -> Status -> Slot
slot person attendance status = Slot person attendance status emptyStat 0

dateRange :: Date -> Int -> [Date]
dateRange _ 0 = []
dateRange date@(year, month, mday) numMeetings = 
  let numDays = 7 * signum numMeetings 
      (y,m,d) = toGregorian $ addDays (toInteger numDays) $ fromGregorian (toInteger year) month mday
      nextMeeting = (fromIntegral y, m, d) :: Date
  in date : dateRange nextMeeting (numMeetings - signum numMeetings)

sameMeeting :: Meeting -> Meeting -> Bool
sameMeeting (Meeting date1 _) (Meeting date2 _) = date1 == date2

honorRequests :: Meeting -> Meeting -> Meeting
honorRequests meeting requests =
  let sortByPerson = sortBy (compare `on` person)
      mergeRequest slot requestSlot = slot {status=(status requestSlot), attendance=(attendance requestSlot)}
      mergeSlots (slot1, slot2) = if (status slot2 == Requested)
                                  then mergeRequest slot1 slot2
                                  else slot1
      slots' = map mergeSlots $ zip (sortByPerson $ slots meeting) (sortByPerson $ slots requests)
  in meeting {slots = slots'}


applyUpdates :: Calendar -> Calendar -> Calendar
applyUpdates calendar updates = sortBy (compare `on` date) $ unionBy sameMeeting updates calendar

mergeRequestCalendar :: Calendar -> Calendar -> Calendar
mergeRequestCalendar calendar requestCalendar = 
  let findMeetingAlways aDate = case findMeeting aDate requestCalendar of
                            Just m -> m
                            Nothing -> Meeting aDate []
      findRequests meeting = fullySlotifyMeeting $ findMeetingAlways (date meeting)
  in map (\meeting -> honorRequests meeting (findRequests meeting)) calendar

rankifyMeeting :: (RandomGen g) => Meeting -> Rand g Meeting
rankifyMeeting meeting = do
  rs <- getRandomRs (1, 100)
  let rankedSlots = map (\(s, r) -> s{rank = r}) $ zip (slots meeting) rs
  return meeting { slots = rankedSlots }

fillInCalendar :: (RandomGen g) => Date -> Int -> Calendar -> Rand g Calendar
fillInCalendar startDate numMeetings calendar = do
  let historyBackCount = -(personCount + 1)
      dates = dateRange startDate numMeetings
      backDates = dateRange startDate historyBackCount

      newMeetingIfMissing :: (RandomGen g) => Date -> Rand g Meeting
      newMeetingIfMissing aDate = do
        case findMeeting aDate calendar of
          Just m -> return m
          Nothing -> rankifyMeeting $ fullySlotifyMeeting $ Meeting aDate []
  calendar' <- mapM newMeetingIfMissing $ union backDates dates
  return $ sortBy (compare `on` date) calendar'
      
updateMeeting :: Calendar -> Calendar -> Date -> Meeting
updateMeeting history calendar aDate =
  let stats = historyStats history
      Just meeting = findMeeting aDate calendar
      meeting' = calcMeeting (length history) $ statifyMeeting meeting
      statifySlot slot = slot {stat = findStat (person slot) stats}
      statifyMeeting m = m {slots = map statifySlot (slots m)}
  in meeting'

calcMeeting :: Int -> Meeting -> Meeting
calcMeeting historyCount  (Meeting date slots) =
  let (present, absent) = partition isPresent slots
                          where isPresent slot = attendance slot /= Absent
      
      (available, confirmed) = partition isAvailable present
                               where isAvailable slot = status slot == Proposed
      rankedAvailable = sortByRank available

      sortByRank       = sortBy (compare `on` rank)
      sortByLastHosted = sortBy (compare `on` lastHostDate . stat )

      (hosts, guests) = if needHost then (eligibleHosts, ineligibleHosts) else ([], rankedAvailable)
                        where needHost                         = not $ any isHost confirmed
                              (eligibleHosts, ineligibleHosts) = choose 1 (sortByLastHosted favoredHosts) (sortByLastHosted unfavoredHosts)
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost rankedAvailable
                              isHost slot                      = attendance slot == Host
                              isFavoredToHost slot             = let personStat = stat slot
                                                                 in (inCount personStat) <= (personCount `div` 2)

      (eligible, notEligible)  = choose numberNeededOut favored unfavored
                                 where numberPresent        = length present
                                       numberConfirmedIn    = length $ filter isIn confirmed
                                       numberConfirmedOut   = length $ filter isOut confirmed
                                       minNumberNeededIn    = ceiling $ numberPresent % 2
                                       numberNeededIn       = minNumberNeededIn - numberConfirmedIn
                                       numberNeededOut      = max 0 $ numberPresent - numberNeededIn - numberConfirmedOut
                                       (favored, unfavored) = partition isFavoredForOut guests
                                       isIn slot            = attendance slot == In || attendance slot == Host
                                       isOut slot           = attendance slot == Out
                                       isFavoredForOut slot = let personStat = stat slot
                                                                  presentCount = historyCount - (absentCount personStat)
                                                                  pctIn = ((inCount personStat) * 100) `div` presentCount
                                                                  pctOut = ((outCount personStat) * 100) `div` presentCount
                                                              in pctOut <= 30 || pctIn >= 60

      newlyOut  = map (\slot -> slot {attendance=Out}) eligible
      newlyIn   = map (\slot -> slot {attendance=In}) notEligible
      newlyHost = map (\slot -> slot {attendance=Host}) hosts

      newSlots    = confirmed ++ absent ++ newlyIn ++ newlyOut ++ newlyHost
      sortedSlots = sortBy (compare `on` person) newSlots
  in  Meeting date sortedSlots

choose :: Int -> [Slot] -> [Slot] -> ([Slot], [Slot])
choose numberNeeded favored unfavored =
  let (chosenFavored, rejectedFavored) = splitAt numberNeeded favored
      numberUnfavoredNeeded = numberNeeded - length chosenFavored
      (chosenUnfavored, rejectedUnfavored) = splitAt numberUnfavoredNeeded unfavored
      chosen = chosenFavored ++ chosenUnfavored
      rejected = rejectedFavored ++ rejectedUnfavored
  in  (chosen, rejected)

fullySlotifyMeeting :: Meeting -> Meeting
fullySlotifyMeeting meeting =
  let allPeople = [minBound .. maxBound] :: [Person]
      allPeopleSlots = map (\p -> slot p TBD Proposed) allPeople
      slotsHaveSamePerson slot1 slot2 = (person slot1) == (person slot2)
      mergedSlots = unionBy slotsHaveSamePerson (slots meeting) allPeopleSlots
  in meeting { slots = mergedSlots }

findMeeting :: Date -> Calendar -> Maybe Meeting
findMeeting aDate calendar = find (\m -> (date m == aDate)) calendar

emptyStat :: Stat
emptyStat = Stat [] [] [] []

findStat :: Person -> Stats -> Stat
findStat = Map.findWithDefault emptyStat

inCount :: Stat -> Int
inCount = length . inDates

outCount :: Stat -> Int
outCount = length . outDates

hostCount :: Stat -> Int
hostCount = length . hostDates

absentCount :: Stat -> Int
absentCount = length . absentDates

lastHostDate :: Stat -> Date
lastHostDate stat = case hostDates stat of
                      []  -> emptyDate
                      ds  -> last ds

gatherHistory :: Date -> Calendar -> Calendar
gatherHistory d calendar = 
  let Just dateIndex = findIndex (\m -> date m == d) calendar
      historyIndex = max 0 (dateIndex - personCount)
      historyCount = dateIndex - historyIndex
  in take historyCount $ drop historyIndex calendar

historyStats :: Calendar -> Stats
historyStats history = 
  let gatherMeetingStats :: Stats -> Meeting -> Stats
      gatherMeetingStats stats (Meeting slotDate slots) = foldl incrementSlotStat stats slots
        where incrementSlotStat :: Stats -> Slot -> Stats
              incrementSlotStat stats slot =
                let key = person slot
                    oldStat = findStat key stats
                    newStat = case attendance slot of
                      In     -> oldStat {inDates = (inDates oldStat) ++ [slotDate]}
                      Out    -> oldStat {outDates = (outDates oldStat) ++ [slotDate]}
                      Host   -> oldStat {inDates = (inDates oldStat) ++ [slotDate], hostDates = (hostDates oldStat) ++ [slotDate]}
                      Absent -> oldStat {absentDates = (absentDates oldStat) ++ [slotDate]}
                      TBD    -> oldStat
                    in Map.alter (\_ -> Just newStat) key stats
      emptyStats = Map.empty :: Stats
  in foldl gatherMeetingStats emptyStats history

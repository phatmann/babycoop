{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Scheduler where

import Data.List
import Data.Function (on)
import Data.Map (Map)
import Data.Time
import Data.Ratio
import Control.Monad.Random
import GHC.Generics
import qualified Data.Map as Map

data Attendance = TBD | In | Out | Host | Absent deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)
data Status = Proposed | Confirmed | Requested deriving (Eq, Show, Generic)
type Year = Int
type Month = Int
type MDay = Int
type Date = (Year, Month, MDay)
type Rank = Int
type Person = String
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
data Calendar = Calendar { persons :: [Person]
                         , meetings :: [Meeting]
                         , title :: String
                         } deriving (Show, Generic)

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

updateMeetings :: Date -> Int -> Calendar -> [Meeting]
updateMeetings startDate numMeetings groupCalendar  =
  let updateMeetings' :: [Meeting] -> [Meeting] -> [Meeting]
      updateMeetings' history [] = []
      updateMeetings' history (m:ms) =
        let meeting = updateMeeting history personCount m
            history' = (drop extra history) ++ [meeting]
            extra = if length history == personCount then 1 else 0
        in meeting : updateMeetings' history' ms
      initialHistory = gatherHistory startDate personCount $ meetings groupCalendar
      personCount = length $ persons groupCalendar
  in updateMeetings' initialHistory $ meetingsAt startDate numMeetings $ meetings groupCalendar

deleteMeetings :: Date -> Int -> [Meeting] -> [Meeting]
deleteMeetings startDate numMeetings meetings =
  let datesToDelete = dateRange startDate numMeetings
      meetingNotinDatesToDelete meeting = not $ (date meeting) `elem` datesToDelete
  in filter meetingNotinDatesToDelete meetings

applyAttendanceUpdates :: Calendar -> Date -> [(Person, Attendance)] -> [Meeting]
applyAttendanceUpdates groupCalendar date [] = meetings groupCalendar
applyAttendanceUpdates groupCalendar date attendanceUpdates = 
  let requestSlots = map (\x -> slot (fst x) (snd x) Requested) attendanceUpdates
  in mergeRequestCalendar groupCalendar [Meeting date requestSlots]

mergeCalendars :: [Meeting] -> [Meeting] -> [Meeting]
mergeCalendars meetings calendar2 = sortBy (compare `on` date) $ unionBy sameMeeting meetings calendar2

applyUpdates :: [Meeting] -> [Meeting] -> [Meeting]
applyUpdates meetings updates = mergeCalendars updates meetings

confirmMeetings :: [Meeting] -> [Meeting]
confirmMeetings meetings = 
  let confirmMeeting (Meeting date slots) = Meeting date $ map confirmSlot slots
      confirmSlot slot = if status slot == Requested then slot else slot{status = Confirmed}
  in map confirmMeeting meetings

honorRequests :: Meeting -> Meeting -> Meeting
honorRequests meeting requests =
  let sortByPerson = sortBy (compare `on` person)
      mergeRequest slot requestSlot = slot {status=newStatus, attendance=newAttendance}
                                        where newAttendance = attendance requestSlot
                                              newStatus = if newAttendance == TBD then Proposed else Requested
      mergeSlots (slot1, slot2) = if (status slot2 == Requested)
                                  then mergeRequest slot1 slot2
                                  else slot1
      slots' = map mergeSlots $ zip (sortByPerson $ slots meeting) (sortByPerson $ slots requests)
  in meeting {slots = slots'}

mergeRequestCalendar :: Calendar -> [Meeting] -> [Meeting]
mergeRequestCalendar groupCalendar requestCalendar = 
  let findMeetingAlways aDate = case findMeeting aDate requestCalendar of
                            Just m -> m
                            Nothing -> Meeting aDate []
      findRequests meeting = fullySlotifyMeeting (persons groupCalendar) $ findMeetingAlways (date meeting)
  in map (\meeting -> honorRequests meeting (findRequests meeting)) $ meetings groupCalendar

rankifyMeeting :: (RandomGen g) => Meeting -> Rand g Meeting
rankifyMeeting meeting = do
  rs <- getRandomRs (1, 100)
  let rankedSlots = map (\(s, r) -> s{rank = r}) $ zip (slots meeting) rs
  return meeting { slots = rankedSlots }

fillInCalendar :: (RandomGen g) => Date -> Int -> Calendar -> Rand g [Meeting]
fillInCalendar startDate numMeetings groupCalendar 
  | (numMeetings <= 0) = return $ meetings groupCalendar
  | otherwise = do
      let dates = dateRange startDate numMeetings
          newMeeting :: (RandomGen g) => Date -> Rand g Meeting
          newMeeting aDate = rankifyMeeting $ fullySlotifyMeeting (persons groupCalendar) $ Meeting aDate []
      calendarAdditions <- mapM newMeeting dates
      return $ mergeCalendars (meetings groupCalendar) calendarAdditions 
           
updateMeeting :: [Meeting] -> Int -> Meeting -> Meeting
updateMeeting history personsCount meeting  =
  let stats = historyStats history
      meeting' = scheduleMeeting (length history) personsCount $ statifyMeeting meeting
      statifyMeeting m = m {slots = map statifySlot (slots m)}
      statifySlot slot = slot {stat = findStat (person slot) stats}
  in meeting'

scheduleMeeting :: Int -> Int -> Meeting -> Meeting
scheduleMeeting historyCount personCount (Meeting date slots) =
  let (present, absent)      = partition isPresent slots
                               where isPresent slot = attendance slot /= Absent
      
      (available, confirmed) = partition isAvailable present
                               where isAvailable slot = status slot == Proposed

      numberOfHosts = 1

      (hosts, guests) = if needHost then (eligibleHosts, ineligibleHosts) else ([], available)
                        where needHost                         = not $ any isHost confirmed
                              sortByLastHosted                 = sortBy (compare `on` lastHostDate . stat )
                              (eligibleHosts, ineligibleHosts) = choose numberOfHosts (sortByLastHosted favoredHosts) (sortByLastHosted unfavoredHosts)
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost available
                              isHost slot                      = attendance slot == Host
                              isFavoredToHost slot             = (inCount $ stat slot) <= (personCount `div` 2)

      (stayingIn, goingOut)  =  let  numberPresent        = length present
                                     numberConfirmedIn    = length $ filter isIn confirmed
                                     minNumberNeededIn    = ceiling $ numberPresent % 2
                                     numberNeededIn       = minNumberNeededIn - numberConfirmedIn - numberOfHosts
                                     sortedGuests         = sortBy (compare `on` boostedRank) guests
                                     boostedRank slot     = if isFavoredForOut slot then r + 1000
                                                              else if isFavoredForIn slot then r - 1000
                                                              else r
                                                            where r = rank slot 
                                     isIn slot            = attendance slot == In
                                     presentCount slot    = historyCount - (absentCount $ stat slot)
                                     pctIn slot           = ((inCount $ stat slot) * 100) `div` (presentCount slot)
                                     pctOut slot          = ((outCount $ stat slot) * 100) `div` (presentCount slot)
                                     isFavoredForOut slot = (pctOut slot) < 30 || (pctIn slot) > 60
                                     isFavoredForIn slot  = (pctOut slot) > 60 || (pctIn slot) < 30
                                in splitAt numberNeededIn sortedGuests 

      newlyIn   = map (\slot -> slot {attendance=In}) stayingIn
      newlyOut  = map (\slot -> slot {attendance=Out}) goingOut
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

fullySlotifyMeeting :: [Person] -> Meeting -> Meeting
fullySlotifyMeeting persons meeting =
  let personSlots = map (\p -> slot p TBD Proposed) persons
      slotsHaveSamePerson slot1 slot2 = (person slot1) == (person slot2)
      mergedSlots = unionBy slotsHaveSamePerson (slots meeting) personSlots
  in meeting { slots = mergedSlots }

findMeeting :: Date -> [Meeting] -> Maybe Meeting
findMeeting aDate meetings = find (\m -> (date m == aDate)) meetings

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

gatherHistory :: Date -> Int -> [Meeting] -> [Meeting]
gatherHistory d personCount meetings = 
  let Just dateIndex = findIndex (\m -> date m == d) meetings
      historyIndex = max 0 (dateIndex - personCount)
      historyCount = dateIndex - historyIndex
  in chunkAt historyIndex historyCount meetings

chunkAt :: Int -> Int -> [a] -> [a]
chunkAt index count xs = take count $ drop index xs

meetingsAt :: Date -> Int -> [Meeting] -> [Meeting]
meetingsAt d count meetings = 
  let Just dateIndex = findIndex (\m -> date m == d) meetings
  in chunkAt dateIndex count meetings

historyStats :: [Meeting] -> Stats
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

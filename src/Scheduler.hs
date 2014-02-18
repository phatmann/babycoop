{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Scheduler (
  Calendar(..),
  Meeting(..),
  Slot(..),
  Status(..),
  Stat,
  Attendance,
  Date,
  Person,
  inCount,
  outCount,
  absentCount,
  lastHostDate,
  confirmPastMeetings,
  extendCalendarIntoFuture,
  findMeeting,
  applyUpdates,
  updateCalendar,
  htf_thisModulesTests
  ) where

import Data.List
import Data.Function (on)
import Data.Map (Map)
import Data.Time
import Data.Ratio
import Data.Maybe
import GHC.Generics
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary, Property, Gen, quickCheck)
import Test.Framework
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
                   } deriving (Show, Generic, Eq)
type Stats = Map Person Stat
data Slot = Slot  { person :: Person
                  , attendance :: Attendance
                  , status :: Status
                  , stat :: Stat
                  , rank :: Rank
                  } deriving (Show, Generic, Eq)
data Meeting = Meeting { date :: Date, slots :: [Slot] } deriving (Show, Generic, Eq)
data Calendar = Calendar { persons :: [Person]
                         , meetings :: [Meeting]
                         , title :: String
                         } deriving (Show, Generic)

emptyDate :: Date
emptyDate = (0, 0, 0)

slot :: Person -> Attendance -> Status -> Slot
slot person attendance status = Slot person attendance status emptyStat 0

futureSpan :: [Person] -> Int
futureSpan persons = (length persons) * 2

dateRange :: Date -> Int -> [Date]
dateRange _ 0 = []
dateRange date@(year, month, mday) numMeetings = 
  let numDays = 7 * signum numMeetings 
      (y,m,d) = toGregorian $ addDays (toInteger numDays) $ fromGregorian (toInteger year) month mday
      nextMeeting = (fromIntegral y, m, d) :: Date
  in date : dateRange nextMeeting (numMeetings - signum numMeetings)

sameMeeting :: Meeting -> Meeting -> Bool
sameMeeting (Meeting date1 _) (Meeting date2 _) = date1 == date2

updateCalendar :: Calendar -> Date -> [(Person, Attendance)] -> Calendar
updateCalendar calendar date attendanceUpdates = do
  let updatedCalendar = calendar { meetings = applyAttendanceUpdates calendar date attendanceUpdates }
      updates         = updateMeetings date (futureSpan $ persons calendar) updatedCalendar
  calendar { meetings = applyUpdates (meetings calendar) updates }

updateMeetings :: Date -> Int -> Calendar -> [Meeting]
updateMeetings startDate numMeetings calendar  =
  let updateMeetings' :: [Meeting] -> [Meeting] -> [Meeting]
      updateMeetings' history [] = []
      updateMeetings' history (m:ms) =
        let meeting = updateMeeting history personCount m
            history' = (drop extra history) ++ [meeting]
            extra = if length history == personCount then 1 else 0
        in meeting : updateMeetings' history' ms
      initialHistory = gatherHistory startDate personCount $ meetings calendar
      personCount = length $ persons calendar
  in updateMeetings' initialHistory $ meetingsAt startDate numMeetings $ meetings calendar

deleteMeetings :: Date -> Int -> [Meeting] -> [Meeting]
deleteMeetings startDate numMeetings meetings =
  let datesToDelete = dateRange startDate numMeetings
      meetingNotinDatesToDelete meeting = not $ (date meeting) `elem` datesToDelete
  in filter meetingNotinDatesToDelete meetings

applyAttendanceUpdates :: Calendar -> Date -> [(Person, Attendance)] -> [Meeting]
applyAttendanceUpdates calendar date [] = meetings calendar
applyAttendanceUpdates calendar date attendanceUpdates = 
  let requestSlots = map (\x -> slot (fst x) (snd x) Requested) attendanceUpdates
  in mergeRequestCalendar calendar [Meeting date requestSlots]

mergeCalendars :: [Meeting] -> [Meeting] -> [Meeting]
mergeCalendars meetings calendar2 = sortBy (compare `on` date) $ unionBy sameMeeting meetings calendar2

applyUpdates :: [Meeting] -> [Meeting] -> [Meeting]
applyUpdates meetings updates = mergeCalendars updates meetings

confirmPastMeetings :: Calendar -> [Meeting] -> Calendar
confirmPastMeetings calendar pastMeetings = 
  let confirmMeeting (Meeting date slots) = Meeting date $ map confirmSlot slots
      confirmSlot slot = if status slot == Requested then slot else slot{status = Confirmed}
      confirmedMeetings = map confirmMeeting pastMeetings
  in calendar { meetings = applyUpdates (meetings calendar) confirmedMeetings }

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
mergeRequestCalendar calendar requestCalendar = 
  let findMeetingAlways aDate = case findMeeting aDate requestCalendar of
                            Just m -> m
                            Nothing -> Meeting aDate []
      findRequests meeting = fullySlotifyMeeting (persons calendar) $ findMeetingAlways (date meeting)
  in map (\meeting -> honorRequests meeting (findRequests meeting)) $ meetings calendar

extendCalendarIntoFuture :: Int -> Calendar -> Gen Calendar
extendCalendarIntoFuture numFutureMeetingsExisting calendar = do
  let numFutureMeetingsRequired = futureSpan $ persons calendar
      numFutureMeetingsNeeded   = numFutureMeetingsRequired - numFutureMeetingsExisting
      lastDate = date $ last $ meetings calendar
  calendar' <- fillInCalendar numFutureMeetingsNeeded calendar 
  return $ updateCalendar calendar' lastDate []

newMeeting :: [Person] -> Date ->  Gen Meeting
newMeeting aPersons aDate  = rankifyMeeting $ fullySlotifyMeeting aPersons $ Meeting aDate []
  where rankifyMeeting :: Meeting -> Gen Meeting
        rankifyMeeting meeting = do
          let rankSlot s = (QC.choose (1,100) :: Gen Rank) >>= (\r -> return s{rank = r})
          rankedSlots <- mapM rankSlot $ slots meeting
          return meeting { slots = rankedSlots }

fillInCalendar :: Int -> Calendar -> Gen Calendar
fillInCalendar numMeetings calendar
  | numMeetings <= 0 = return calendar
  | otherwise        = do
                        let dates = tail $ dateRange lastDate (numMeetings + 1)
                            lastDate = date $ last $ meetings calendar

                        calendarAdditions <- mapM (newMeeting $ persons calendar) dates
                        let extendedMeetings = mergeCalendars (meetings calendar) calendarAdditions 
                        return $ calendar {meetings = extendedMeetings}
           
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
                              (eligibleHosts, ineligibleHosts) = chooseFavored numberOfHosts (sortByLastHosted favoredHosts) (sortByLastHosted unfavoredHosts)
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost available
                              isHost slot                      = attendance slot == Host
                              isFavoredToHost slot             = (inCount $ stat slot) <= (personCount `div` 2)

      (stayingIn, goingOut)  =  let  numberPresent        = length present
                                     numberConfirmedIn    = length $ filter (\slot -> attendance slot == In) confirmed
                                     minNumberNeededIn    = ceiling $ numberPresent % 2
                                     numberNeededIn       = minNumberNeededIn - numberConfirmedIn - numberOfHosts
                                     sortedGuests         = sortBy (compare `on` boostedRank) guests
                                     boostedRank slot     
                                      | historyCount == 0 = rank slot
                                      | otherwise         =  if isFavoredForOut slot then r + 1000
                                                               else if isFavoredForIn slot then r - 1000
                                                               else r
                                                             where r = rank slot 
                                     
                                     isFavoredForOut slot = (pctOut slot) < 30 || (pctIn slot) > 60
                                     isFavoredForIn slot  = (pctOut slot) > 60 || (pctIn slot) < 30
                                     presentCount slot    = historyCount - (absentCount $ stat slot)
                                     pctIn slot           = ((inCount $ stat slot) * 100) `div` (presentCount slot)
                                     pctOut slot          = ((outCount $ stat slot) * 100) `div` (presentCount slot)
                                in splitAt numberNeededIn sortedGuests 

      newlyIn   = map (\slot -> slot {attendance=In}) stayingIn
      newlyOut  = map (\slot -> slot {attendance=Out}) goingOut
      newlyHost = map (\slot -> slot {attendance=Host}) hosts

      newSlots    = confirmed ++ absent ++ newlyIn ++ newlyOut ++ newlyHost
      sortedSlots = sortBy (compare `on` person) newSlots
  in  Meeting date sortedSlots

chooseFavored :: Int -> [Slot] -> [Slot] -> ([Slot], [Slot])
chooseFavored numberNeeded favored unfavored =
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

----------------------------
-- TESTS
----------------------------
prop_fillInEmptyCalendarLength :: Property
prop_fillInEmptyCalendarLength = QC.forAll sampleCalendar prop_fillInCalendarLength

prop_fillInCalendarLength :: Calendar -> Property
prop_fillInCalendarLength calendar = QC.forAll (QC.choose (0, 6)) test
  where test :: Int -> Gen Bool
        test numMeetings = do 
          numMeetings <- QC.choose(0, 6)
          calendar'   <- fillInCalendar numMeetings calendar
          return $ (length $ meetings calendar') == (length $ meetings calendar) + numMeetings

prop_confirmPast :: Calendar -> Property
prop_confirmPast calendar =  QC.forAll (numMeetingsFromCalendar calendar) test
  where test :: Int -> Gen Bool
        test splitPoint = do
          let pastMeetings = drop splitPoint (meetings calendar)
              calendar' = confirmPastMeetings calendar pastMeetings
              allSlotsConfirmed meeting = all (\s -> status s == Confirmed || status s == Requested) $ slots meeting
              meetingAtDateConfirmed date = allSlotsConfirmed meeting
                where meeting = fromJust $ findMeeting date (meetings calendar')
          return $ all (\m -> meetingAtDateConfirmed $ date m) pastMeetings

prop_extendCalendarLength :: Calendar -> Property
prop_extendCalendarLength calendar = QC.forAll (numMeetingsFromCalendar calendar) test
  where test :: Int -> Gen Bool
        test numFutureMeetingsExisting  = do
          let numFutureMeetingsRequired = futureSpan $ persons calendar
              numFutureMeetingsNeeded   = numFutureMeetingsRequired - numFutureMeetingsExisting
              expectedNewCalendarLength =
                (length $ meetings calendar) + if numFutureMeetingsNeeded > 0 then numFutureMeetingsNeeded else 0

          calendar' <- extendCalendarIntoFuture numFutureMeetingsExisting calendar
          return $ (length $ meetings calendar') == expectedNewCalendarLength

prop_meetingOneHost :: Meeting -> Gen Bool
prop_meetingOneHost meeting = return $ (numWithAttendance Host meeting) == 1

prop_meetingHasAtLeastHalfIn :: Meeting -> Gen Bool
prop_meetingHasAtLeastHalfIn meeting = return $ (numWithAttendance In meeting) + (numWithAttendance Host meeting) >= ((length $ slots meeting) `div` 2)

prop_meetingHasAtMostHalfOut :: Meeting -> Gen Bool
prop_meetingHasAtMostHalfOut meeting = return $ (numWithAttendance Out meeting) <= ((length $ slots meeting) `div` 2)

numWithAttendance aAttendance meeting = length . filter (\slot -> attendance slot == aAttendance) $ slots meeting

numMeetingsFromCalendar :: Calendar -> Gen Int
numMeetingsFromCalendar calendar = QC.choose(0, length $ meetings calendar)

sampleCalendar :: Gen Calendar
sampleCalendar = do
  let ps = ["Person1", "Person2", "Person3", "Person4", "Person5", "Person6"]
  meeting <- newMeeting ps (2014,1,1)
  return Calendar {
             title    = "Sample Calendar"
            ,persons  = ps
            ,meetings = [meeting]
         }

instance Arbitrary Calendar where
  arbitrary =  do
    numMeetings <- QC.choose (0, 20)
    calendar    <- sampleCalendar
    fillInCalendar numMeetings calendar   

instance Arbitrary Meeting where
  arbitrary =  do
    calendar <- arbitrary :: Gen Calendar
    meeting  <- elements $ meetings calendar
    let meeting' = head $ updateMeetings (date meeting) 1 calendar
    return meeting'

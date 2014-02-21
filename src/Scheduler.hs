{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Scheduler (
  Calendar(..),
  Meeting(..),
  Slot(..),
  Status(..),
  Stat(..),
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
  emptyStat,
  recentHistoryCount,
  htf_thisModulesTests
  ) where

import Data.List
import Data.Function (on)
import Data.Map (Map)
import Data.Time
import Data.Ratio
import Data.Maybe
import GHC.Generics
import Debug.Trace
import Control.Applicative
import Test.QuickCheck
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
                  , recentStat :: Stat
                  , rank :: Rank
                  } deriving (Show, Generic, Eq)
data Meeting = Meeting { date :: Date,  historyCount :: Int, slots :: [Slot]} deriving (Show, Generic, Eq)
data Calendar = Calendar { persons :: [Person]
                         , meetings :: [Meeting]
                         , title :: String
                         } deriving (Show, Generic)

emptyDate :: Date
emptyDate = (0, 0, 0)

slot :: Person -> Attendance -> Status -> Slot
slot person attendance status = Slot person attendance status emptyStat emptyStat 0

futureSpan :: [Person] -> Int
futureSpan persons = (length persons) * 2

recentHistoryCount = 2

dateRange :: Date -> Int -> [Date]
dateRange _ 0 = []
dateRange date@(year, month, mday) numMeetings = 
  let numDays = 7 * signum numMeetings 
      (y,m,d) = toGregorian $ addDays (toInteger numDays) $ fromGregorian (toInteger year) month mday
      nextMeeting = (fromIntegral y, m, d) :: Date
  in date : dateRange nextMeeting (numMeetings - signum numMeetings)

sameMeeting :: Meeting -> Meeting -> Bool
sameMeeting (Meeting date1 _ _) (Meeting date2 _ _) = date1 == date2

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
        let meeting = updateMeeting history m
            history' = (drop extra history) ++ [meeting]
            extra = if length history == historyCount then 1 else 0
        in meeting : updateMeetings' history' ms
      initialHistory = gatherHistory startDate historyCount $ meetings calendar
      historyCount   = (length $ persons calendar) * 2
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
  in mergeRequestCalendar calendar [Meeting date 0 requestSlots]

mergeCalendars :: [Meeting] -> [Meeting] -> [Meeting]
mergeCalendars meetings calendar2 = sortBy (compare `on` date) $ unionBy sameMeeting meetings calendar2

applyUpdates :: [Meeting] -> [Meeting] -> [Meeting]
applyUpdates meetings updates = mergeCalendars updates meetings

confirmPastMeetings :: Calendar -> [Meeting] -> Calendar
confirmPastMeetings calendar pastMeetings = 
  let confirmMeeting (Meeting date historyCount slots) = Meeting date historyCount $ map confirmSlot slots
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
                            Nothing -> Meeting aDate 0 []
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
newMeeting aPersons aDate  = rankifyMeeting $ fullySlotifyMeeting aPersons $ Meeting aDate 0 []
  where rankifyMeeting :: Meeting -> Gen Meeting
        rankifyMeeting meeting = do
          let rankSlot s = (choose (1,100) :: Gen Rank) >>= (\r -> return s{rank = r})
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
           
updateMeeting :: [Meeting] -> Meeting -> Meeting
updateMeeting history meeting  =
  let recentHistory  = drop ((length history) - recentHistoryCount) history
      stats             = historyStats history
      recentStats    = historyStats recentHistory
      statifyMeeting m  = m {slots = map statifySlot (slots m), historyCount = length history}
      statifySlot slot  = slot { stat          = findStat (person slot) stats,
                                 recentStat = findStat (person slot) recentStats }
  in scheduleMeeting $ statifyMeeting meeting

scheduleMeeting :: Meeting -> Meeting
scheduleMeeting (Meeting date historyCount slots) =
  let (present, absent)      = partition isPresent slots
                               where isPresent slot = attendance slot /= Absent
      
      (available, confirmed) = partition isAvailable present
                               where isAvailable slot = status slot == Proposed

      numberOfHosts = 1
      personCount   = length slots 

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
                                      | otherwise         = rank slot
                                                              + if tooManyRecentOuts slot then (-10000) else 0
                                                              + if tooManyRecentIns slot then 10000 else 0
                                                              + if isFavoredForOut slot then 1000 else 0
                                                              + if isFavoredForIn slot then (-1000) else 0
                                     
                                     isFavoredForOut slot      = (pctOut slot) < 30 || (pctIn slot) > 60 
                                     isFavoredForIn slot       = (pctOut slot) > 60 || (pctIn slot) < 30
                                     tooManyRecentOuts slot    = (outCount $ recentStat slot) == recentHistoryCount
                                     tooManyRecentIns slot     = (inCount $ recentStat slot)  == recentHistoryCount
                                     presentCount slot         = historyCount - (absentCount $ stat slot)
                                     pctIn slot                = ((inCount $ stat slot) * 100) `div` (presentCount slot)
                                     pctOut slot               = ((outCount $ stat slot) * 100) `div` (presentCount slot)
                                in splitAt numberNeededIn sortedGuests 

      newlyIn   = map (\slot -> slot {attendance=In}) stayingIn
      newlyOut  = map (\slot -> slot {attendance=Out}) goingOut
      newlyHost = map (\slot -> slot {attendance=Host}) hosts

      newSlots    = confirmed ++ absent ++ newlyIn ++ newlyOut ++ newlyHost
      sortedSlots = sortBy (compare `on` person) newSlots
  in  Meeting date historyCount sortedSlots

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
inCount stat = (length $ inDates stat) + (length $ hostDates stat)

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
gatherHistory d historyCount meetings = 
  let Just dateIndex = findIndex (\m -> date m == d) meetings
      historyIndex = max 0 (dateIndex - historyCount)
      historyCount' = dateIndex - historyIndex
  in chunkAt historyIndex historyCount' meetings

chunkAt :: Int -> Int -> [a] -> [a]
chunkAt index count xs = take count $ drop index xs

meetingsAt :: Date -> Int -> [Meeting] -> [Meeting]
meetingsAt d count meetings = 
  let Just dateIndex = findIndex (\m -> date m == d) meetings
  in chunkAt dateIndex count meetings

historyStats :: [Meeting] -> Stats
historyStats history = 
  let gatherMeetingStats :: Stats -> Meeting -> Stats
      gatherMeetingStats stats (Meeting slotDate _ slots) = foldl incrementSlotStat stats slots
        where incrementSlotStat :: Stats -> Slot -> Stats
              incrementSlotStat stats slot =
                let key = person slot
                    oldStat = findStat key stats
                    newStat = case attendance slot of
                      In     -> oldStat {inDates     = (inDates oldStat) ++ [slotDate]}
                      Out    -> oldStat {outDates    = (outDates oldStat) ++ [slotDate]}
                      Host   -> oldStat {hostDates   = (hostDates oldStat) ++ [slotDate]}
                      Absent -> oldStat {absentDates = (absentDates oldStat) ++ [slotDate]}
                      TBD    -> oldStat
                    in Map.alter (\_ -> Just newStat) key stats
      emptyStats = Map.empty :: Stats
  in foldl gatherMeetingStats emptyStats history

----------------------------
-- TESTS
----------------------------
prop_fillInEmptyCalendarLength :: Property
prop_fillInEmptyCalendarLength = forAll sampleCalendar prop_fillInCalendarLength

prop_fillInCalendarLength :: Calendar -> Property
prop_fillInCalendarLength calendar = forAll (choose (0, 6)) test
  where test :: Int -> Gen Bool
        test numMeetings = do 
          numMeetings <- choose(0, 6)
          calendar'   <- fillInCalendar numMeetings calendar
          return $ (length $ meetings calendar') == (length $ meetings calendar) + numMeetings

prop_confirmPast :: Calendar -> Property
prop_confirmPast calendar =  forAll (numMeetingsFromCalendar calendar) test
  where test :: Int -> Gen Bool
        test splitPoint = do
          let pastMeetings = drop splitPoint (meetings calendar)
              calendar' = confirmPastMeetings calendar pastMeetings
              allSlotsConfirmed meeting = all (\s -> status s == Confirmed || status s == Requested) $ slots meeting
              meetingAtDateConfirmed date = allSlotsConfirmed meeting
                where meeting = fromJust $ findMeeting date (meetings calendar')
          return $ all (\m -> meetingAtDateConfirmed $ date m) pastMeetings

prop_extendCalendarLength :: Calendar -> Property
prop_extendCalendarLength calendar = forAll (numMeetingsFromCalendar calendar) test
  where test :: Int -> Gen Bool
        test numFutureMeetingsExisting  = do
          let numFutureMeetingsRequired = futureSpan $ persons calendar
              numFutureMeetingsNeeded   = numFutureMeetingsRequired - numFutureMeetingsExisting
              expectedNewCalendarLength =
                (length $ meetings calendar) + if numFutureMeetingsNeeded > 0 then numFutureMeetingsNeeded else 0

          calendar' <- extendCalendarIntoFuture numFutureMeetingsExisting calendar
          return $ (length $ meetings calendar') == expectedNewCalendarLength

prop_firstMeetingIsValid :: Property
prop_firstMeetingIsValid = forAll firstMeeting $ (\m -> conjoin [
  property $ prop_meetingWasScheduled m,
  property $ prop_meetingOneHost m,
  property $ prop_meetingHasAtLeastHalfIn m,
  property $ prop_meetingHasAtMostHalfOut m,
  property $ prop_meetingHistoryCount m])

prop_meetingWasScheduled :: Meeting -> Gen Bool
prop_meetingWasScheduled meeting = return $ all (\s -> attendance s /= TBD) $ slots meeting

prop_meetingOneHost :: Meeting -> Gen Bool
prop_meetingOneHost meeting = return $ (numWithAttendance Host meeting) == 1

prop_meetingHasAtLeastHalfIn :: Meeting -> Gen Bool
prop_meetingHasAtLeastHalfIn meeting = return $ (numWithAttendance In meeting) + (numWithAttendance Host meeting) >= ((length $ slots meeting) `div` 2)

prop_meetingHasAtMostHalfOut :: Meeting -> Gen Bool
prop_meetingHasAtMostHalfOut meeting = return $ (numWithAttendance Out meeting) <= ((length $ slots meeting) `div` 2)

prop_meetingHistoryCount :: Meeting -> Property
prop_meetingHistoryCount meeting = whenFail printFailMsg $ all statMatchesHistoryCount $ stat <$> slots meeting
  where statMatchesHistoryCount stat = (statSum stat) == (historyCount meeting)
        statSum stat = sum $ map (\f -> length $ f stat) [hostDates, inDates, outDates, absentDates]
        printFailMsg = mapM_ (print . statSum) (stat <$> slots meeting)

prop_noPersonHasSameAttendanceThreeConsecutiveMeetings :: Calendar -> Property
prop_noPersonHasSameAttendanceThreeConsecutiveMeetings calendar = forAll (dateFromCalendar calendar) test
  where test :: Date -> Property
        test date = do
          let threeMeetings = meetingsAt date 3 $ meetings calendar
              stats = snd <$> (Map.toList $ historyStats threeMeetings)
              printAttendance = print $ map (\s -> (person s, attendance s)) <$> slots <$> threeMeetings
          whenFail printAttendance $ all (\s -> (length $ inDates s) < 3 && outCount s < 3) stats


---------------------------
-- Generators and Helpers
---------------------------

tracef m f = trace (m ++ ": " ++ show f) f

numWithAttendance aAttendance meeting = length . filter (\slot -> attendance slot == aAttendance) $ slots meeting

numMeetingsFromCalendar :: Calendar -> Gen Int
numMeetingsFromCalendar calendar = choose (0, length $ meetings calendar)

dateFromCalendar :: Calendar -> Gen Date
dateFromCalendar calendar = elements $ date <$> meetings calendar

sampleCalendar :: Gen Calendar
sampleCalendar = do
  let ps = ["Person1", "Person2", "Person3", "Person4", "Person5", "Person6"]
  meeting <- newMeeting ps (2014,1,1)
  return Calendar {
             title    = "Sample Calendar"
            ,persons  = ps
            ,meetings = [meeting]
         }

firstMeeting = do
    calendar <- arbitrary :: Gen Calendar
    return $ head $ meetings calendar 

instance Arbitrary Calendar where
  arbitrary =  do
    numExtraMeetings <- choose (0, 12)
    calendar         <- sampleCalendar
    calendar'        <- fillInCalendar numExtraMeetings calendar 
    let startDate   = date $ head $ meetings calendar'
        numMeetings = length $ meetings calendar'
        updates     = updateMeetings startDate numMeetings calendar'
    return $ calendar { meetings = applyUpdates (meetings calendar) updates }

instance Arbitrary Meeting where
  arbitrary =  do
    calendar <- arbitrary :: Gen Calendar
    elements $ meetings calendar

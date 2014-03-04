{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Scheduler (
  Calendar(..),
  Meeting(..),
  Slot(..),
  Status(..),
  Stat(..),
  Attendance(..),
  Hosting(..),
  Date,
  Person,
  Weekday,
  inCount,
  outCount,
  absentCount,
  lastHostDate,
  confirmPastMeetings,
  extendCalendarIntoFuture,
  findMeeting,
  applyUpdates,
  updateCalendar,
  updateMeetingWeekday,
  emptyStat,
  recentHistoryCount,
  htf_thisModulesTests
  ) where

import Data.List
import Data.Function (on)
import Data.Map (Map)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Ratio
import Data.Maybe
import GHC.Generics
import Debug.Trace
import Control.Applicative
import Test.QuickCheck
import Test.Framework
import qualified Data.Map as Map

data Attendance = XHost | TBD | In | Out | Absent deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)
data Status = Proposed | Confirmed | Requested deriving (Eq, Show, Generic)
type Year = Int
type Month = Int
type MDay = Int
type Date = (Year, Month, MDay)
type Weekday = Int -- 1 = Monday, 7 = Sunday
type Rank = Int
type Person = String
data Stat = Stat   { inDates :: [Date]
                   , outDates :: [Date]
                   , hostDates :: [Date]
                   , absentDates :: [Date]
                   } deriving (Show, Generic, Eq)
type Stats = Map Person Stat
data Hosting = WillHost | WontHost | CanHost deriving (Show, Generic, Eq)
data Slot = Slot  { person :: Person
                  , attendance :: Attendance
                  , hosting :: Hosting
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

slot :: Person -> Attendance -> Hosting -> Status -> Slot
slot person attendance hosting status = Slot person attendance hosting status emptyStat emptyStat 0

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

updateCalendar :: Calendar -> Date -> [(Person, Attendance, Hosting)] -> Calendar
updateCalendar calendar date attendanceUpdates =
  let updatedCalendar = calendar { meetings = applyAttendanceUpdates calendar date attendanceUpdates }
      updates         = updateMeetings date (futureSpan $ persons calendar) updatedCalendar
  in calendar { meetings = applyUpdates (meetings calendar) updates }

updateMeetingWeekday :: Date -> Weekday -> Calendar -> Calendar
updateMeetingWeekday d weekday calendar  = 
  let ms = meetings calendar
      Just dateIndex = findIndex (\m -> date m == d) ms
      (oldMeetings, newMeetings) = splitAt dateIndex ms
      newMeetings' = changeMeetingDay weekday newMeetings 
  in calendar { meetings = oldMeetings ++ newMeetings'}

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

applyAttendanceUpdates :: Calendar -> Date -> [(Person, Attendance, Hosting)] -> [Meeting]
applyAttendanceUpdates calendar date [] = meetings calendar
applyAttendanceUpdates calendar date attendanceUpdates = 
  let  requestSlots = map (\(person, attendance, hosting) -> slot person attendance hosting Requested) attendanceUpdates
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
      mergeRequest slot requestSlot = slot {status=newStatus, attendance=newAttendance, hosting=newHosting}
                                        where newAttendance = attendance requestSlot
                                              newHosting    = hosting requestSlot
                                              isRequest     = newAttendance /= TBD && newHosting /= CanHost
                                              newStatus     = if isRequest then Requested else Proposed
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
                        where needHost                         = not $ any (\s -> hosting s == WillHost) confirmed
                              sortByLastHosted                 = sortBy (compare `on` lastHostDate . stat )
                              (eligibleHosts, ineligibleHosts) = chooseFavored numberOfHosts (sortByLastHosted favoredHosts) (sortByLastHosted unfavoredHosts)
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost available
                              isFavoredToHost slot             = (inCount $ stat slot) <= (personCount `div` 2)

      (stayingIn, goingOut)  =  let  numberPresent        = length present
                                     numberConfirmedIn    = length $ filter (\slot -> attendance slot == In) confirmed
                                     minNumberNeededIn    = ceiling $ numberPresent % 2
                                     numberNeededIn       = minNumberNeededIn - numberConfirmedIn
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

      newlyIn   = map (\slot -> slot {hosting=WontHost, attendance=In}) stayingIn
      newlyOut  = map (\slot -> slot {hosting=WontHost, attendance=Out}) goingOut
      newlyHost = map (\slot -> slot {hosting=WillHost, attendance=In}) hosts

      newSlots    = confirmed ++ absent ++ newlyIn ++ newlyOut ++ newlyHost
      sortedSlots = sortBy (compare `on` person) newSlots
  in  Meeting date historyCount sortedSlots

meetingWeekday :: Meeting -> Weekday
meetingWeekday Meeting {date=(y,m,d)} = weekday
  where (_, _, weekday) = toWeekDate $ fromGregorian (toInteger y) m d

changeMeetingDay :: Weekday -> [Meeting] -> [Meeting]
changeMeetingDay newWeekday meetings  = 
  let currentWeekday = meetingWeekday $ head meetings
      delta          = toInteger $ newWeekday - currentWeekday
      
      shiftDate (y, m, d) = 
        let (y',m',d') = toGregorian $ addDays delta $ fromGregorian (toInteger y) m d
        in (fromIntegral y', m', d') :: Date

  in map (\m -> m {date = shiftDate $ date m}) meetings



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
  let personSlots = map (\p -> slot p TBD CanHost Proposed) persons
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
                      Absent -> oldStat {absentDates = (absentDates oldStat) ++ [slotDate]}
                      TBD    -> oldStat
                    newStat' = if hosting slot == WillHost
                               then newStat {hostDates = (hostDates oldStat) ++ [slotDate]}
                               else newStat
                    in Map.alter (\_ -> Just newStat') key stats
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
prop_meetingOneHost meeting = return $ (numHosting meeting) == 1

prop_meetingHasAtLeastHalfIn :: Meeting -> Gen Bool
prop_meetingHasAtLeastHalfIn meeting = return $ (numWithAttendance In meeting) >= ((length $ slots meeting) `div` 2)

prop_meetingHasAtMostHalfOut :: Meeting -> Gen Bool
prop_meetingHasAtMostHalfOut meeting = return $ (numWithAttendance Out meeting) <= ((length $ slots meeting) `div` 2)

prop_meetingHistoryCount :: Meeting -> Property
prop_meetingHistoryCount meeting = whenFail printFailMsg $ all statMatchesHistoryCount $ stat <$> slots meeting
  where statMatchesHistoryCount stat = (statSum stat) == (historyCount meeting)
        statSum stat = sum $ map (\f -> length $ f stat) [inDates, outDates, absentDates]
        printFailMsg = mapM_ (print . statSum) (stat <$> slots meeting)

prop_noPersonHasSameAttendanceThreeConsecutiveMeetings :: Calendar -> Property
prop_noPersonHasSameAttendanceThreeConsecutiveMeetings calendar = forAll (dateFromCalendar calendar) test
  where test :: Date -> Property
        test date = do
          let threeMeetings = meetingsAt date 3 $ meetings calendar
              stats = snd <$> (Map.toList $ historyStats threeMeetings)
              printAttendance = print $ map (\s -> (person s, attendance s)) <$> slots <$> threeMeetings
          whenFail printAttendance $ all (\s -> (length $ inDates s) < 3 && outCount s < 3) stats

-- prop_hostRotatesProperly = undefined

prop_canChangeMeetingDay :: Calendar -> Property
prop_canChangeMeetingDay calendar =
  forAll (choose (1,7)) (\weekday ->
    forAll (meetingsFromCalendar calendar) $ test weekday)
  where test :: Weekday -> [Meeting] -> Gen Bool
        test weekday meetings = 
            let meetings' = changeMeetingDay weekday meetings
            in return $ all (\m -> (meetingWeekday m) == weekday) meetings'

prop_updateMeetingWeekday :: Calendar -> Property
prop_updateMeetingWeekday calendar = forAll (choose (1,7)) (\weekday ->
    forAll (dateFromCalendar calendar) $ test weekday)
  where test :: Weekday -> Date -> Gen Bool
        test weekday d = 
            let ms = meetings calendar
                Just index = findIndex (\m -> date m == d) ms 
                (oldMeetings, newMeetings) = splitAt index ms 
                calendar' = updateMeetingWeekday d weekday calendar
                (oldMeetings', newMeetings') = splitAt index $ meetings calendar' 
            in return $ (all (\m -> (meetingWeekday m) == weekday) newMeetings') &&
                        (oldMeetings == oldMeetings')





---------------------------
-- Generators and Helpers
---------------------------

tracef m f = trace (m ++ ": " ++ show f) f

numWithAttendance :: Attendance -> Meeting -> Int
numWithAttendance aAttendance meeting = length . filter (\slot -> attendance slot == aAttendance) $ slots meeting

numHosting :: Meeting -> Int
numHosting meeting = length . filter (\slot -> (hosting slot) == WillHost)  $ slots meeting

numMeetingsFromCalendar :: Calendar -> Gen Int
numMeetingsFromCalendar calendar = choose (0, length $ meetings calendar)

dateFromCalendar :: Calendar -> Gen Date
dateFromCalendar calendar = elements $ date <$> meetings calendar

meetingsFromCalendar :: Calendar -> Gen [Meeting]
meetingsFromCalendar calendar = do
  let ms = meetings calendar
      len  = length ms
  index <- choose (0, len - 1)
  count <- choose (1, len - index)
  return $ take count $ drop index ms

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

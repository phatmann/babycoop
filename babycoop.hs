module BabyCoop where

import Data.List
import Debug.Trace
import Data.Function (on)
import System.Random
import Shuffle
import Calendar

maxChosen = 3
maxIns    = 2
maxOuts   = 2
historyCount = 6

type History = [Week]

{-
Rules:
# If more than 3 persons are available, remove whomever went out the week prior
# If still more than 3 persons are available, remove whomever went out 2 weeks prior
# If still more than 3 persons are available, randomly choose 3
# Only 2 can reserve a week
# Never babysit more than 2 weeks in a row
# Hosted never chosen
# Rotate who hosts
-}

updateCalendar :: StdGen -> Calendar -> Calendar
updateCalendar randGen calendar = let emptyDate = (0, 0, 0)
                                      emptyWeek = (emptyDate, [])
                                      emptyHistory = take historyCount $ repeat emptyWeek
                                   in updateCalendar' emptyHistory randGen calendar

updateCalendar' :: History -> StdGen -> Calendar -> Calendar
updateCalendar' history randGen [] = []
updateCalendar' history randGen (week:remainingWeeks) = let (updatedWeek, updatedRandGen) = updateWeek history randGen week
                                                            updatedHistory = (drop 1 history) ++ [updatedWeek]
                                                        in  updatedWeek : updateCalendar' updatedHistory updatedRandGen remainingWeeks

updateWeek :: History -> StdGen -> Week -> (Week, StdGen)
updateWeek history randGen (date, slots) = let  (available, unavailable) = partition (\slot -> attendance slot == TBD) slots
                                                isFavored slot = let (inCount, outCount) = inOutCount history (person slot)
                                                                        in inCount > maxIns || outCount < maxOuts
                                                (shuffledAvailable, updatedRandGen) = shuffle available randGen
                                                (favored, unfavored) = partition isFavored shuffledAvailable
                                                (eligible, notEligible) = partitionEligible favored unfavored
                                                chosen = map (setAttendance Out) eligible
                                                notChosen = map (setAttendance In) (notEligible ++ unavailable)
                                                sortedSlots = sortBy (compare `on` attendance) (chosen ++ notChosen)
                                            in  ((date, sortedSlots), updatedRandGen)

partitionEligible :: [Slot] -> [Slot] -> ([Slot], [Slot])
partitionEligible favored unfavored = let (eligibleFavored, ineligibleFavored) = splitAt maxChosen favored
                                          numberEligibleNeeded = maxChosen - length eligibleFavored
                                          (eligibleUnfavored, ineligibleUnfavored) = splitAt numberEligibleNeeded unfavored
                                      in  (eligibleFavored ++ eligibleUnfavored, ineligibleFavored ++ ineligibleUnfavored)

inOutCount :: [Week] -> Person -> (Int, Int)
inOutCount history person = let statusHistory = map (lookupAttendance person) history
                                wasOut status = status == Out
                                wasIn status  = status `elem` [In, Host]
                                testStatus inOutAcc status  = (if wasIn status then fst inOutAcc + 1 else fst inOutAcc,
                                                          if wasOut status then snd inOutAcc + 1 else snd inOutAcc)
                            in  foldl testStatus (0, 0) statusHistory

lookupAttendance :: Person -> Week -> Attendance
lookupAttendance targetPerson (date,slots) =  let s = find (\slot -> (person slot) == targetPerson) slots 
                                              in case s of
                                                Nothing   -> Absent
                                                Just slot -> attendance slot

setAttendance :: Attendance -> Slot -> Slot
setAttendance newAttendance slot
  | newAttendance == Out && (attendance slot)  == TBD  = slot {attendance=Out}
  | newAttendance == In  && (attendance slot)  == TBD  = slot {attendance=In}
  | newAttendance == Out && (attendance slot)  /= TBD  = slot
  | newAttendance == In  && (attendance slot)  /= TBD  = slot
  | otherwise                                          = slot {attendance=newAttendance}

printSlot :: Slot -> IO ()
printSlot slot = do putStrLn ((show $ person slot) ++ ":" ++ (show $ attendance slot))

printWeek :: Week -> IO ()
printWeek (date@(year, month, day), slots) = do putStrLn ((show month) ++ "/" ++ (show day))
                                                mapM_ printSlot slots
                                                putStrLn ""

main = do randGen <- newStdGen
          mapM_ printWeek $ updateCalendar randGen theCalendar
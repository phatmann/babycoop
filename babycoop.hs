module BabyCoop where

import Data.List
import Debug.Trace
import Data.Function (on)
import System.Random
import Shuffle
import Calendar

maxInsPerWeek      = 3
maxInsPerPerson    = 2
maxOutsPerPerson   = 2
historyCount       = 6

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
updateWeek history randGen (date, slots) = let  (present, absent) = partition isPresent slots
                                                (available, confirmed) = partition isAvailable present
                                                neededIn = maxInsPerWeek - (length $ filter isIn confirmed)
                                                (shuffledAvailable, updatedRandGen) = shuffle available randGen
                                                (favored, unfavored) = partition isFavored shuffledAvailable
                                                (eligible, notEligible) = partitionEligible neededIn favored unfavored
                                                newlyOut = map (\slot -> slot {attendance=Out}) eligible
                                                newlyIn = map (\slot -> slot {attendance=In}) notEligible
                                                sortedSlots = sortBy (compare `on` attendance) (confirmed ++ absent ++ newlyIn ++ newlyOut)
                                                isPresent slot = attendance slot /= Absent
                                                isAvailable slot = status slot == Proposed
                                                isIn slot = attendance slot /= Out
                                                isFavored slot = let (inCount, outCount) = inOutCount history (person slot)
                                                                 in inCount > maxInsPerPerson || outCount < maxOutsPerPerson
                                            in  ((date, sortedSlots), updatedRandGen)

partitionEligible :: Int -> [Slot] -> [Slot] -> ([Slot], [Slot])
partitionEligible neededIn favored unfavored =  let (eligibleFavored, ineligibleFavored) = splitAt neededIn favored
                                                    numberUnfavoredEligibleNeeded = neededIn - length eligibleFavored
                                                    (eligibleUnfavored, ineligibleUnfavored) = splitAt numberUnfavoredEligibleNeeded unfavored
                                                in  (eligibleFavored ++ eligibleUnfavored, ineligibleFavored ++ ineligibleUnfavored)

inOutCount :: [Week] -> Person -> (Int, Int)
inOutCount history person = let attendanceHistory = map (lookupAttendance person) history
                                outCount = length $ filter (\attendance -> attendance == Out) attendanceHistory
                                inCount  = length $ filter (\attendance -> attendance `elem` [In, Host]) attendanceHistory
                            in  (inCount, outCount)

lookupAttendance :: Person -> Week -> Attendance
lookupAttendance targetPerson (date,slots) =  let result = find (\slot -> (person slot) == targetPerson) slots 
                                              in case result of
                                                Nothing   -> Absent
                                                Just slot -> attendance slot
printSlot :: Slot -> IO ()
printSlot slot = do putStrLn ((show $ person slot) ++ ":" ++ (show $ attendance slot))

printWeek :: Week -> IO ()
printWeek (date@(year, month, day), slots) = do putStrLn ((show month) ++ "/" ++ (show day))
                                                mapM_ printSlot slots
                                                putStrLn ""

main' = do randGen <- newStdGen
           mapM_ printWeek $ updateCalendar randGen theCalendar
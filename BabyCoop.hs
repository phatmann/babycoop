module BabyCoop where

import Data.List
import Debug.Trace
import Data.Function (on)
import System.Random
import Shuffle
import Calendar

personCount = (+1) $ fromEnum $ (maxBound :: Person) 

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
                                      emptyHistory = take personCount $ repeat emptyWeek
                                   in updateCalendar' emptyHistory randGen calendar

updateCalendar' :: History -> StdGen -> Calendar -> Calendar
updateCalendar' history randGen [] = []
updateCalendar' history randGen (week:remainingWeeks) =
  let (updatedWeek, updatedRandGen) = updateWeek history randGen week
      updatedHistory                = (drop 1 history) ++ [updatedWeek]
  in  updatedWeek : updateCalendar' updatedHistory updatedRandGen remainingWeeks

updateWeek :: History -> StdGen -> Week -> (Week, StdGen)
updateWeek history randGen (date, slots) =
  let (present, absent) = partition isPresent slots
                          where isPresent slot = attendance slot /= Absent

      (available, confirmed) = partition isAvailable present
                               where isAvailable slot = status slot == Proposed

      (shuffledAvailable, updatedRandGen) = shuffle available randGen

      (hosts, guests) = if needHost then (eligibleHosts, ineligibleHosts) else ([], shuffledAvailable)
                        where needHost                         = not $ any isHost confirmed
                              (eligibleHosts, ineligibleHosts) = partitionEligible 1 favoredHosts unfavoredHosts
                              (favoredHosts, unfavoredHosts)   = partition isFavoredToHost shuffledAvailable
                              isHost slot                      = attendance slot == Host
                              isFavoredToHost slot             = let (_, inCount, hostCount) = inOutHostCount history (person slot)
                                                                 in hostCount == 0 && inCount <= (personCount `div` 2)

      (eligible, notEligible)  = partitionEligible numberEligibleNeeded favored unfavored
                                 where numberEligibleNeeded = max 0 $ (personCount `div` 2) - (length $ filter isIn confirmed)
                                       (favored, unfavored) = partition isFavoredForOut guests
                                       isIn slot            = attendance slot == In || attendance slot == Host
                                       historyCount         = length history
                                       isFavoredForOut slot = let (inCount, outCount, _) = inOutHostCount history (person slot)
                                                              in inCount >= (historyCount `div` 2) || outCount < (historyCount `div` 2)

      newlyOut  = map (\slot -> slot {attendance=Out}) eligible
      newlyIn   = map (\slot -> slot {attendance=In}) notEligible
      newlyHost = map (\slot -> slot {attendance=Host}) hosts

      newSlots    = confirmed ++ absent ++ newlyIn ++ newlyOut ++ newlyHost
      sortedSlots = sortBy (compare `on` attendance) newSlots
  in  ((date, sortedSlots), updatedRandGen)

partitionEligible :: Int -> [Slot] -> [Slot] -> ([Slot], [Slot])
partitionEligible numberEligibleNeeded favored unfavored =
  let (eligibleFavored, ineligibleFavored)      = splitAt numberEligibleNeeded favored
      numberUnfavoredEligibleNeeded             = numberEligibleNeeded - length eligibleFavored
      (eligibleUnfavored, ineligibleUnfavored)  = splitAt numberUnfavoredEligibleNeeded unfavored
  in  (eligibleFavored ++ eligibleUnfavored, ineligibleFavored ++ ineligibleUnfavored)

-- TODO: do one pass through attendance history to gather all stats

inOutHostCount :: [Week] -> Person -> (Int, Int, Int)
inOutHostCount history person =
  let attendanceHistory = map (lookupAttendance person) history
      inCount           = length $ filter (\attendance -> attendance `elem` [In, Host]) attendanceHistory
      outCount          = length $ filter (\attendance -> attendance == Out) attendanceHistory
      hostCount         = length $ filter (\attendance -> attendance == Host) attendanceHistory
  in  (inCount, outCount, hostCount)

lookupAttendance :: Person -> Week -> Attendance
lookupAttendance targetPerson (date,slots) =
  let result = find (\slot -> (person slot) == targetPerson) slots 
  in case result of
    Nothing   -> Absent
    Just slot -> attendance slot

------------------

printSlot :: Slot -> IO ()
printSlot slot = do putStrLn ((show $ person slot) ++ ":" ++ (show $ attendance slot))

printWeek :: Week -> IO ()
printWeek (date@(year, month, day), slots) = do putStrLn ((show month) ++ "/" ++ (show day))
                                                mapM_ printSlot slots
                                                putStrLn ""

maix = do randGen <- newStdGen
          mapM_ printWeek $ updateCalendar randGen theCalendar

s :: IO ()
s = do randGen <- newStdGen
       mapM_ print $ updateCalendar randGen theCalendar
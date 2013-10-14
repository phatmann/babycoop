module Main where

import Data.List (sortBy, partition)
import Data.Function (on)



data Status = Available | Unavailable | Hosting | Requested | Rejected | Chosen | NotChosen deriving (Eq, Show)
type Date = (Int, Int, Int) -- year, month, date
type Person = String
type Slot = (Person, Status)
type Week = (Date, [Slot])
type Calendar = [Week]

maxChosen = 3
maxOuts   = 1
historyCount = 2
persons = ["Rebecca", "Jenny", "Kate", "Kasey", "Neha", "Erica"]
initialSlots = map (\x -> (x, Available)) persons

theCalendar = [((2013, 10, 14), initialSlots), ((2013, 10, 21), initialSlots), ((2013, 10, 28), initialSlots)]

{-
Rules:
# If more than 3 persons are available, remove whomever went out the week prior
# If still more than 3 persons are available, remove whomever went out 2 weeks prior
# If still more than 3 persons are available, randomly choose 3
# Only 2 can reserve a week
# Hosted never chosen
# Rotate who hosts
-}




emptyDate = (0, 0, 0)
emptySlot = (emptyDate, [])
emptyHistory = take historyCount $ repeat emptySlot

updateCalendar :: Calendar -> Calendar
updateCalendar calendar = updateCalendar' emptyHistory calendar

updateCalendar' :: [Week] -> Calendar -> Calendar
updateCalendar' history [] = []
updateCalendar' history (week:remainingWeeks) = let updatedWeek = updateWeek history week
                                                    updatedHistory = (drop 1 history) ++ [updatedWeek]
                                                in  updatedWeek : updateCalendar' updatedHistory remainingWeeks

updateWeek :: [Week] -> Week -> Week
updateWeek history (date, avails) = let rankedSlots           = map (pairWithOutCount history) avails
                                        sortedRankedSlots     = sortBy (compare `on` fst) rankedSlots
                                        groupedRankedSlots    = partition (isEligible) sortedRankedSlots
                                        eligibleRankedSlots   = fst groupedRankedSlots
                                        ineligibleRankedSlots = snd groupedRankedSlots
                                        (limitedEligibleRankedSlots, restEligibleRankedSlots) = splitAt maxChosen eligibleRankedSlots
                                        chooseRankedSlot (rank, (person, status)) = (rank, (person, Chosen))
                                        unchooseRankedSlot (rank, slot@(person, Available)) = (rank, (person, NotChosen))
                                        unchooseRankedSlot rankedSlot = rankedSlot
                                        chosenRankedSlots   = map chooseRankedSlot limitedEligibleRankedSlots
                                        notChosenRankedSlots = map unchooseRankedSlot $ restEligibleRankedSlots ++ ineligibleRankedSlots
                                        allRankedSlots      = chosenRankedSlots ++ notChosenRankedSlots
                                    in  (date, map snd allRankedSlots)

pairWithOutCount :: [Week] -> Slot -> (Int, Slot)
pairWithOutCount history slot@(person, status) =  let statusHistory = map (lookupStatus person) history
                                                      wasOut status = status `elem` [Chosen, Requested]
                                                      outCount = length $ filter wasOut statusHistory
                                                  in  (outCount, slot)

isEligible :: (Int, Slot) -> Bool
isEligible (outCount, slot@(person, status)) = status == Available && outCount <= maxOuts -- TODO: correct?

lookupStatus :: Person -> Week -> Status
lookupStatus person week@(_, slots) =  let s = lookup person slots
                                                in case s of
                                                  Nothing     -> Unavailable
                                                  Just status -> status


main = do print $ updateCalendar theCalendar



--          mapM_ print_week $ zip [1..] $ randPerm gen $ split_into_groupings 3 moms
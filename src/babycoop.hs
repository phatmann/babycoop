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

-- Kasey, Neha, *Rebecca NotChosen 10/14
-- *Kate, Erica, Jenny NotChosen 10/21

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
updateWeek history (date, slots) = let  sorted = sortBy (compare `on` outCount history) slots
                                        (eligible, ineligible) = partition (isEligible) sorted
                                        (limitedEligible, restEligible) = splitAt maxChosen eligible
                                        chooseSlot (person, status) = (person, Chosen)
                                        unchooseSlot (person, Available) = (person, NotChosen)
                                        unchooseSlot slot = slot
                                        chosen = map chooseSlot limitedEligible
                                        notChosen = map unchooseSlot $ restEligible ++ ineligible
                                    in  (date, chosen ++ notChosen)

outCount :: [Week] -> Slot -> Int
outCount history slot@(person, status) =  let statusHistory = map (lookupStatus person) history
                                              wasOut status = status `elem` [Chosen, Requested]
                                          in  length $ filter wasOut statusHistory

isEligible :: Slot -> Bool
isEligible slot@(person, status) = status == Available -- TODO: correct?

lookupStatus :: Person -> Week -> Status
lookupStatus person week@(_, slots) =  let s = lookup person slots
                                                in case s of
                                                  Nothing     -> Unavailable
                                                  Just status -> status


main = do print $ updateCalendar theCalendar



--          mapM_ print_week $ zip [1..] $ randPerm gen $ split_into_groupings 3 moms
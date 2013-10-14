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
updateWeek history (date, slots) = let  (available, unavailable) = partition (\(person, status) -> status == Available) slots
                                        (favored, unfavored) = partition (\slot -> (outCount history slot) < maxOuts) (shuffle available)
                                        (eligible, notEligible) = partitionEligible favored unfavored
                                        chosen = map (setStatus Chosen) eligible
                                        notChosen = map (setStatus NotChosen) (notEligible ++ unavailable)
                                    in  (date, chosen ++ notChosen)

partitionEligible :: [Slot] -> [Slot] -> ([Slot], [Slot])
partitionEligible favored unfavored = let (eligibleFavored, ineligibleFavored) = splitAt maxChosen favored
                                          numberEligibleNeeded = maxChosen - length eligibleFavored
                                          (eligibleUnfavored, ineligibleUnfavored) = splitAt numberEligibleNeeded unfavored
                                      in  (eligibleFavored ++ eligibleUnfavored, ineligibleFavored ++ ineligibleUnfavored)

outCount :: [Week] -> Slot -> Int
outCount history slot@(person, status) =  let statusHistory = map (lookupStatus person) history
                                              wasOut status = status `elem` [Chosen, Requested]
                                          in  length $ filter wasOut statusHistory

shuffle :: [a] -> [a]
shuffle = id -- TODO

lookupStatus :: Person -> Week -> Status
lookupStatus person week@(_, slots) =  let s = lookup person slots
                                                in case s of
                                                  Nothing     -> Unavailable
                                                  Just status -> status

setStatus :: Status -> Slot -> Slot
setStatus Chosen (person, Available)     = (person, Chosen)
setStatus NotChosen (person, Available)  = (person, NotChosen)
setStatus Chosen slot  = slot
setStatus NotChosen slot  = slot
setStatus newStatus (person, status)  = (person, newStatus)

main = do print $ updateCalendar theCalendar



--          mapM_ print_week $ zip [1..] $ randPerm gen $ split_into_groupings 3 moms
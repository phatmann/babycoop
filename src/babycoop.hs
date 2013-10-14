module Main where

import Data.List (sortBy, partition)
import Data.Function (on)

persons = ["Rebecca", "Jenny", "Kate", "Kasey", "Neha", "Erica"]

data Status = Available | Unavailable | Hosting | Requested | Rejected | Chosen | NotChosen deriving (Eq, Show)
type Date = (Int, Int, Int) -- year, month, date
type Person = String
type Availability = (Person, Status)
type Week = (Date, [Availability])
type Calendar = [Week]

theCalendar = [((2013, 10, 14), [("Rebecca", Available), ("Jenny", Available), ("Kate", Available), ("Kasey", Available), ("Neha", Available), ("Erica", Available)])]

{-
Rules:
# If more than 3 persons are available, remove whomever went out the week prior
# If still more than 3 persons are available, remove whomever went out 2 weeks prior
# If still more than 3 persons are available, randomly choose 3
# Only 2 can reserve a week
# Hosted never chosen
# Rotate who hosts
-}


maxChosen = 3
maxOuts   = 1
historyCount = 2

emptyDate = (0, 0, 0)
emptyAvail = (emptyDate, [])
emptyHistory = take historyCount $ repeat emptyAvail

updateCalendar :: Calendar -> Calendar
updateCalendar calendar = updateCalendar' emptyHistory calendar

updateCalendar' :: [Week] -> Calendar -> Calendar
updateCalendar' history [] = []
updateCalendar' history (week:remainingWeeks) = let updatedWeek = updateWeek history week
                                                    updatedHistory = (drop 1 history) ++ [updatedWeek]
                                                in  updatedWeek : updateCalendar' updatedHistory remainingWeeks

updateWeek :: [Week] -> Week -> Week
updateWeek history (date, avails) = let rankedAvails           = map (pairWithOutCount history) avails
                                        sortedRankedAvails     = sortBy (compare `on` fst) rankedAvails
                                        groupedRankedAvails    = partition (isEligible) sortedRankedAvails
                                        eligibleRankedAvails   = fst groupedRankedAvails
                                        ineligibleRankedAvails = snd groupedRankedAvails
                                        (limitedEligibleRankedAvails, restEligibleRankedAvails) = splitAt maxChosen eligibleRankedAvails
                                        chooseRankedAvailability (rank, (person, status)) = (rank, (person, Chosen))
                                        chosenRankedAvails   = map chooseRankedAvailability limitedEligibleRankedAvails
                                        allRankedAvails      = chosenRankedAvails ++ restEligibleRankedAvails ++ ineligibleRankedAvails
                                    in  (date, map snd allRankedAvails)

pairWithOutCount :: [Week] -> Availability -> (Int, Availability)
pairWithOutCount history availability@(person, status) =  let statusHistory = map (lookupStatus person) history
                                                              wasOut status = status `elem` [Chosen, Requested]
                                                              outCount = length $ filter wasOut statusHistory
                                                          in  (outCount, availability)

isEligible :: (Int, Availability) -> Bool
isEligible (outCount, availability@(person, status)) = status == Available && outCount <= maxOuts -- TODO: correct?

lookupStatus :: Person -> Week -> Status
lookupStatus person week@(_, availabilities) =  let s = lookup person availabilities
                                                in case s of
                                                  Nothing     -> Unavailable
                                                  Just status -> status


main = do print $ updateCalendar theCalendar



--          mapM_ print_week $ zip [1..] $ randPerm gen $ split_into_groupings 3 moms
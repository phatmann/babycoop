module Main where

import Data.List
import Debug.Trace
import Data.Function (on)
import System.Random
import Shuffle

data Status = Available | Unavailable | Hosting | Requested | Rejected | Chosen | NotChosen deriving (Eq, Ord)
type Date = (Int, Int, Int) -- year, month, date
type Person = String
type Slot = (Person, Status)
type Week = (Date, [Slot])
type Calendar = [Week]

maxChosen = 3
maxIns    = 2
maxOuts   = 2
historyCount = 6
persons = ["Rebecca", "Jenny", "Kate", "Kasey", "Neha", "Erica"]
allAvailable = map (\x -> (x, Available)) persons

theCalendar = [((2013, 10, 14),
                  [("Rebecca", Hosting),
                   ("Kasey",   NotChosen),
                   ("Neha",    NotChosen),
                   ("Kate",    Chosen),
                   ("Erica",   Chosen),
                   ("Jenny",   Chosen)]),
               ((2013, 10, 21), 
                  [("Rebecca", Chosen),
                   ("Kasey",   Chosen),
                   ("Neha",    Chosen),
                   ("Kate",    Hosting),
                   ("Erica",   NotChosen),
                   ("Jenny",   NotChosen)]),
               ((2013, 10, 28), allAvailable),
               ((2013, 11, 4),  allAvailable),
               ((2013, 11, 11), allAvailable),
               ((2013, 11, 18), allAvailable),
               ((2013, 11, 25), allAvailable)]

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
                                      emptySlot = (emptyDate, [])
                                      emptyHistory =  take historyCount $ repeat emptySlot
                                   in updateCalendar' emptyHistory randGen calendar

updateCalendar' :: [Week] -> StdGen -> Calendar -> Calendar
updateCalendar' history randGen [] = []
updateCalendar' history randGen (week:remainingWeeks) = let (updatedWeek, updatedRandGen) = updateWeek history randGen week
                                                            updatedHistory = (drop 1 history) ++ [updatedWeek]
                                                        in  updatedWeek : updateCalendar' updatedHistory updatedRandGen remainingWeeks

updateWeek :: [Week] -> StdGen -> Week -> (Week, StdGen)
updateWeek history randGen (date, slots) = let  (available, unavailable) = partition (\(person, status) -> status == Available) slots
                                                isFavored (person, _) = let counts = inOutCount history person
                                                                        in (fst counts) > maxIns || (snd counts) < maxOuts
                                                (shuffledAvailable, updatedRandGen) = shuffle available randGen
                                                (favored, unfavored) = partition isFavored shuffledAvailable
                                                (eligible, notEligible) = partitionEligible favored unfavored
                                                chosen = map (setStatus Chosen) eligible
                                                notChosen = map (setStatus NotChosen) (notEligible ++ unavailable)
                                                sortedSlots = sortBy (compare `on` snd) (chosen ++ notChosen)
                                            in  ((date, sortedSlots), updatedRandGen)

partitionEligible :: [Slot] -> [Slot] -> ([Slot], [Slot])
partitionEligible favored unfavored = let (eligibleFavored, ineligibleFavored) = splitAt maxChosen favored
                                          numberEligibleNeeded = maxChosen - length eligibleFavored
                                          (eligibleUnfavored, ineligibleUnfavored) = splitAt numberEligibleNeeded unfavored
                                      in  (eligibleFavored ++ eligibleUnfavored, ineligibleFavored ++ ineligibleUnfavored)

inOutCount :: [Week] -> Person -> (Int, Int)
inOutCount history person = let statusHistory = map (lookupStatus person) history
                                wasOut status = status `elem` [Chosen, Requested]
                                wasIn status  = status `elem` [NotChosen, Rejected, Hosting]
                                testStatus acc status  = (if wasIn status then fst acc + 1 else fst acc,
                                                          if wasOut status then snd acc + 1 else snd acc)
                            in  foldl testStatus (0, 0) statusHistory

lookupStatus :: Person -> Week -> Status
lookupStatus person (_, slots) =  let s = lookup person slots
                                  in case s of
                                    Nothing     -> Unavailable
                                    Just status -> status

setStatus :: Status -> Slot -> Slot
setStatus Chosen (person, Available) = (person, Chosen)
setStatus NotChosen (person, Available) = (person, NotChosen)
setStatus Chosen slot  = slot
setStatus NotChosen slot  = slot
setStatus newStatus (person, status) = (person, newStatus)

instance Show Status where
  show Available   = "Available"
  show Unavailable = "Unavailable"
  show Chosen      = "Out"
  show NotChosen   = "Babysitting"
  show Hosting     = "Babysitting (Hosting)"
  show Requested   = "Out (Requested)"
  show Rejected    = "Babysitting (Requested)"

printSlot :: Slot -> IO ()
printSlot slot@(person, status) = do putStrLn (person ++ ":" ++ (show status))

printWeek :: Week -> IO ()
printWeek (date@(year, month, day), slots) = do putStrLn ((show month) ++ "/" ++ (show day))
                                                mapM_ printSlot slots
                                                putStrLn ""

main = do randGen <- newStdGen
          mapM_ printWeek $ updateCalendar randGen theCalendar
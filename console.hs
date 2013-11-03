module Console where

import Calendar
import System.Random

printSlot :: Slot -> IO ()
printSlot slot = do putStrLn ((show $ person slot) ++ ":" ++ (show $ attendance slot))

printWeek :: Week -> IO ()
printWeek (date@(year, month, day), slots) = do putStrLn ((show month) ++ "/" ++ (show day))
                                                mapM_ printSlot slots
                                                putStrLn ""

main :: IO ()
main = do -- randGen <- newStdGen
          let stats = gatherStats (2013, 11, 4) theCalendar
          print $ show stats
          --mapM_ printWeek $ updateCalendar randGen theCalendar
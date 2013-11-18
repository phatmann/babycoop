module Console where

import Calendar

-- import Text.Show.Pretty
-- import qualified Data.Map as Map

--showDate :: Date -> String
--showDate (0, 0, 0) = "not recently"
--showDate (year, month, day) = (show month) ++ "/" ++ (show day) ++ "/" ++ (show year)

--printSlot :: Slot -> IO ()
--printSlot slot = do putStrLn $ (show $ person slot) ++ ": " ++ (show $ attendance slot) ++ (showStat $ stat slot)

--printMeeting :: Meeting -> IO ()
--printMeeting (Meeting d slots) = do putStrLn $ showDate d
--                                    mapM_ printSlot slots
--                                    putStrLn ""
                                                
--showStat :: Stat -> String
--showStat stat =  
--  let inStr      = show $ inCount stat
--      outStr     = show $ outCount stat
--      absentStr  = show $ absentCount stat
--      lastHosted = showDate $ lastHostDate stat
--  in " (in=" ++ inStr ++ ", out=" ++ outStr ++ ", absent=" ++ absentStr ++ ", lastHosted=" ++ lastHosted ++ ")"

main :: IO ()
main = updateCalendar (2013, 12, 3) []
  
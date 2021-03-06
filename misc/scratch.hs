import Data.List
import Data.Time.Calendar
import System.Random

type Group  = [Person]
type Grouping = (Group, Group)

split_into_groupings :: Int -> [Name] -> [Grouping]
split_into_groupings 0 xs     = [([], xs)]
split_into_groupings n []     = []
split_into_groupings n (x:xs) = x_groups ++ xs_groups
  where
    x_groups     = [ (x:group1,group2) | (group1,group2) <- split_into_groupings (n-1) xs ]
    xs_groups    = [ (group1,x:group2) | (group1,group2) <- split_into_groupings n xs ]

print_group :: [Char] -> Group -> IO ()
print_group name group = do
  putStr name
  putStrLn $ intercalate ", " group

print_week :: (Int, Grouping) -> IO ()
print_week (week_number, grouping@(group1, group2)) = do
  putStrLn $ "Week " ++ show week_number
  print_group "Babysit: " group1
  print_group "Go out: " group2
  putStrLn ""

isAvailable :: Status -> Bool
isAvailable status = status == Available || status == Requested

priorWeek :: Date -> Date
priorWeek date@(y, m, d) = toGregorian priorSysDay 
                             where sysDay = fromGregorian y m d
                                   weekAgo = -7
--                                   priorSysDay = addDays weekAgo sysDay

--http://www.haskell.org/haskellwiki/Random_shuffle

module Shuffle where

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
 
-- | Randomly shuffle a list without the IO Monad
--   /O(N)/

shuffle :: [a] -> StdGen -> ([a],StdGen)
shuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

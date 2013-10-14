module RandPerm where

import System.Random

randPerm :: StdGen -> [a] -> [a]
randPerm _ []   = []
randPerm gen xs = let (n,newGen) = randomR (0, length xs -1) gen
                      front = xs !! n
                  in  front : randPerm newGen (take n xs ++ drop (n+1) xs)
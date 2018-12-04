-- Day 1: Chronal Calibration
-- https://adventofcode.com/2018/day/1

import qualified Data.IntSet as IntSet

frequencies :: (Num a) => [a] -> [a]
frequencies = scanl (+) 0 . cycle

firstDuplicate :: [Int] -> Int
firstDuplicate = dup IntSet.empty 
    where
        dup prev (x:xs) = if IntSet.member x prev
                          then x
                          else dup (IntSet.insert x prev) xs
        dup _ _ = error "Reached the edge of infinity!"


main = do
    inStr <- getContents
    let numbers = map (\x -> read x :: Int)
                . map (dropWhile (== '+'))
                . lines
                $ inStr
    
    putStrLn $ "Problem 1 - part 1: " 
            ++ (show $ sum numbers)
    putStrLn $ "Problem 1 - part 2: "            
            ++ (show . firstDuplicate $ frequencies numbers)


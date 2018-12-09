-- Day 5: Alchemical Reduction
-- https://adventofcode.com/2018/day/5

import Data.Char (toUpper)
import Data.List (nub)

reaction :: [Char] -> [Char]
reaction = foldr react ""
    where
        react x (y:ys) | opposite x y = ys
                       | otherwise    = x:y:ys
        react x [] = x:[]
        opposite a b = (a /= b) && (toUpper a == toUpper b)

repair :: [Char] -> Int
repair str = minimum $ map length candidates
    where
        polymers = nub $ map (toUpper) str
        candidates = [ reaction $ filter (\x -> (toUpper x) /= y) str | y <- polymers ]


main = do
    inStr <- getContents
    let polymer = head . lines $ inStr

    putStrLn $ "Problem 4 - part 1: "
            ++ (show . length $ reaction polymer)
    putStrLn $ "Problem 4 - part 2: "
            ++ (show $ repair polymer)
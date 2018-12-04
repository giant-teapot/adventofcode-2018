-- Day 2: Inventory Management System
-- https://adventofcode.com/2018/day/2

import Data.List (group, sort, tails)

hasNOfAnyElem :: (Ord a) => Int -> [a] -> Bool
hasNOfAnyElem n = any (==n) . map length . group . sort

inventoryChecksum :: (Ord a) => [[a]] -> Int
inventoryChecksum xs = doubles * triples
    where
        doubles = length $ filter (hasNOfAnyElem 2) xs
        triples = length $ filter (hasNOfAnyElem 3) xs


distance :: (Eq a) => [a] -> [a] -> Int
distance x y = length . filter (uncurry (/=)) $ zip x y

match :: (Eq a) => [a] -> [a] -> [a]
match x y = map fst . filter (uncurry (==)) $ zip x y

matchingIds :: (Eq a) => [[a]] -> [a]
matchingIds xs =
    uncurry match . head $ filter ((==) 1 . uncurry distance) pairs
    where
        pairs = [ (x,y) | (x:rest) <- tails xs , y <- rest ]


main = do
    inStr <- getContents
    let boxIds = lines $ inStr
    
    putStrLn $ "Problem 2 - part 1: "
            ++ (show $ inventoryChecksum boxIds)
    putStrLn $ "Problem 2 - part 2: "
            ++ (show $ matchingIds boxIds)

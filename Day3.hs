-- Day 3: No Matter How You Slice It
-- https://adventofcode.com/2018/day/3

import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List ((\\), groupBy, nub, sortOn)

data Claim = Claim
    { id :: Int
    , x  :: Int
    , y  :: Int
    , w  :: Int
    , h  :: Int
    } deriving (Show, Eq)

parseClaim :: [Char] -> Maybe Claim
parseClaim = makeClaim . catMaybes . map readMaybe . words . cleanLine
    where
        cleanLine = map (\x -> if x `elem` separators then ' ' else x)
        separators = ['#', '@', ':', ',', 'x']
        makeClaim [id, x, y, w, h] = Just (Claim id x y w h)
        makeClaim _ = Nothing

coords :: Claim -> [(Int, Int, Int)]
coords (Claim id x y w h) = [(id, i,j) | i <- [x..x+w-1], j <- [y..y+h-1]]

groupContestedCells ::  [(Int, Int, Int)] -> [[(Int, Int, Int)]]
groupContestedCells = filter ((>1) . length)
                    . groupBy cmpCoords
                    . sortOn getCoords
    where
        getCoords (_,x,y) = (x,y)
        cmpCoords (_, x1, y1) (_, x2, y2) = (x1, y1) == (x2, y2)

contestedIds ::  [(Int, Int, Int)] -> [Int]
contestedIds =  nub . map getId
    where
        getId (id, _, _) = id


main = do
    inStr <- getContents
    let claims = catMaybes
               . map parseClaim
               . lines
               $ inStr
    let contestedCells = groupContestedCells . concat $ map coords claims

    putStrLn $ "Problem 3 - part 1: "
            ++ (show . length $ contestedCells)
    putStrLn $ "Problem 3 - part 2: "
            ++ (show ([1..length claims] \\ (contestedIds $ concat contestedCells)))

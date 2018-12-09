-- Day 4: Repose Record
-- https://adventofcode.com/2018/day/4

import Data.Function (on)
import Data.List (sort, sortBy, group, groupBy)
import Data.Ord
import Data.Time (LocalTime)
import Data.Time.Format (defaultTimeLocale, readSTime)
import Data.Time.LocalTime (todMin, localTimeOfDay)

newtype Guard = Guard { guardId :: Int }
    deriving (Eq, Ord, Show)

data Event = Shift Guard
           | Wake
           | Sleep
    deriving (Show)

parseLogLine :: [Char] -> (LocalTime, Event)
parseLogLine str = case readDate str of
    [(t, e)] -> (t, parseEvent e)
    _        -> error str
    where
        readDate = readSTime True defaultTimeLocale "[%F %R]"

parseEvent :: [Char] -> Event
parseEvent str = case words str of
    ["Guard", '#':id, "begins", "shift"] -> Shift (Guard (read id))
    ["wakes", "up"]                      -> Wake
    ["falls", "asleep"]                  -> Sleep
    _                                    -> error str

parseLogFile :: [String] -> [(LocalTime, Event)]
parseLogFile = sortBy (comparing fst) . map parseLogLine

sleepMinutes :: [(LocalTime, Event)] -> [(Guard, Int)]
sleepMinutes = expandSpans . spans (error "")
    where
        spans _ ((_, Shift g):xs) = spans g xs
        spans g ((t1, Sleep):(t2, Wake):xs) = (g,t1,t2):spans g xs
        spans _ [] = []
        spans _ xs = error ("Missing Shift before " ++ show xs)

        expandSpans xs = [(g, t) | (g, from, to) <- xs
                                 , t <- [getMinute from .. getMinute to - 1]]
        getMinute = todMin . localTimeOfDay

sleepyGuard1 :: [(LocalTime, Event)] -> Int
sleepyGuard1 eventLog = (guardId guard) * mostSleptMinute
    where
        (guard, minutes) = (\xs -> (fst . head $ xs, map snd xs))
                         . head
                         . sortBy (comparing (Down . length))
                         . groupBy ((==) `on` fst)
                         . sortBy (comparing fst) 
                         $ sleepMinutes eventLog
        mostSleptMinute = head . head
                        . sortBy (comparing (Down . length))
                        . group . sort $ minutes

sleepyGuard2 :: [(LocalTime, Event)] -> Int
sleepyGuard2 eventLog = (guardId guard) * minute
    where
        (guard, minute) = head . head
                        . sortBy (comparing (Down . length))
                        . group
                        . sort
                        $ sleepMinutes eventLog


main = do
    inStr <- getContents
    let events = parseLogFile . lines $ inStr

    putStrLn $ "Problem 4 - part 1: "
            ++ (show $ sleepyGuard1 events)
    putStrLn $ "Problem 4 - part 2: "
            ++ (show $ sleepyGuard2 events)

import Day1
import Day10
import Day11
import Day12
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import System.Environment

daylookup :: String -> String -> (String, String)
daylookup day dayinput =
  let days =
        [ ("day12", day12),
          ("day11", day11),
          ("day10", day10),
          ("day9", day9),
          ("day8", day8),
          ("day7", day7),
          ("day6", day6),
          ("day5", day5),
          ("day4", day4),
          ("day3", day3),
          ("day2", day2),
          ("day1", day1)
        ]
   in case lookup day days of
        Just dfn -> dfn dayinput
        Nothing -> error "Wrong Day"

solve :: (String, String) -> String
solve (part1, part2) = unlines ["Part 1 : " ++ part1, "Part 2 : " ++ part2]

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 then return () else error "Incorrect syntax"
  input <- readFile "testcase.txt"
  putStr . solve $ (daylookup . head) args input

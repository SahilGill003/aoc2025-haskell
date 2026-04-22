module Day6
( day6
) where 

import Data.List

solve :: [String] -> [[String]] -> Integer
solve [] _ = 0
solve _ [] = 0
solve ops oprs = 
  let op = head ops
      opp x y = if op == "+" then x + y else x * y
      res = foldl f (if op == "+" then 0 else 1) (head oprs) 
              where f x y = opp x (read y :: Integer)
  in (res + solve (tail ops) (tail oprs))

merge :: [[String]] -> [[String]]
merge [] = []
merge str
  | null scnd || scnd == [[""]] = [concat frst]
  | otherwise = concat frst : merge (tail scnd)
  where (frst, scnd) = break (== [""]) str

parse :: [String] ->[[String]]
parse [] = []
parse str
  | null $ head str = []
  | otherwise = [nums] : parse nstr
  where nstr = map tail str 
        nums = reverse $ foldl (\x y -> if head y /= ' ' then head y : x else x) [] str 

part1 :: String -> String
part1 v = let oprs = (transpose . map words . init . lines) v
              ops = (words . last . lines) v
          in show $ solve ops oprs


part2 :: String -> String
part2 v = let ops = (words . last . lines) v
          in (show . solve ops . merge . parse . init . lines) v
          -- in show  [length ops, length oprs, length $ head oprs]

day6 :: String -> (String, String)
day6 str = (part1 str, part2 str)

module Day3
  ( day3,
  )
where

import Data.Char(ord)

findIndex :: [Int] -> Int -> Int
findIndex [] _ = -1
findIndex (h : t) x = if h == x then 0 else 1 + findIndex t x

_turnOn :: [Int] -> Int -> Int -> Int
_turnOn [] _ _ = 0
_turnOn bank nbat num
  | nbat == 1 = num * 10 + maximum bank
  | len >= nbat = _turnOn (drop idx bank) (nbat - 1) (num * 10 + v)
  | otherwise = 0
  where
    len = length bank
    m = take (len - nbat + 1) bank
    v = maximum m
    idx = succ $ findIndex bank v

turnOnk :: Int -> String -> Int
turnOnk nbat bank = _turnOn (map (\x -> ord x - ord '0') bank) nbat 0

part1 :: String -> String
part1 = show . sum . map (turnOnk 2) . lines

part2 :: String -> String
part2 = show . sum . map (turnOnk 12) . lines

day3 :: String -> (String, String)
day3 str = (part1 str, part2 str)

module Day10
  ( day10,
  )
where

import Data.Bits

splitBy :: (Char -> Bool) -> [Char] -> [String]
splitBy isCh = foldr f [[]]
  where
    f x y =
      if isCh x
        then (if null (head y) then y else [] : y)
        else (x : head y) : tail y

type Machine = (Int, [Int], [Int])

parseBraces :: String -> [Int]
parseBraces str =
  let nstr = drop 1 $ init str
      vals = map (\x -> read x :: Int) $ splitBy (== ',') nstr
   in vals

parseString :: [String] -> Machine
parseString str =
  let temp = (drop 1 $ init (head str))
      diags = configMap 0 temp
      buttons = map (buttonMap 0 0 (length temp) . parseBraces) (drop 1 $ init str)
      jolts = parseBraces (last str)
   in (diags, buttons, jolts)

buttonMap :: Int -> Int -> Int -> [Int] -> Int
buttonMap val id1 maxi arr
  | null arr = if id1 < maxi then shiftL val (maxi - id1) else val
  | id1 == id2 = buttonMap (shiftL val 1 + 1) (succ id1) maxi (tail arr)
  | otherwise = buttonMap (shiftL val 1) (succ id1) maxi arr
  where
    id2 = head arr

configMap :: Int -> String -> Int
configMap val str
  | null str = val
  | h == '#' = configMap (shiftL val 1 + 1) (tail str)
  | otherwise = configMap (shiftL val 1) (tail str)
  where
    h = head str

click :: Int -> Int -> Int
click = xor

checkSubsets :: [Int] -> Int -> Int -> Int -> Int
checkSubsets nums curr used target
  | curr == target = used
  | null nums = 100
  | otherwise = min skip once
  where
    v = head nums
    skip = checkSubsets (tail nums) curr used target
    once = checkSubsets (tail nums) (click v curr) (used + 1) target

fewestPresses :: Machine -> Int
fewestPresses (diag, btns, _) = checkSubsets btns 0 0 diag

part1 :: String -> String
part1 v =
  let raw = map (splitBy (== ' ')) $ lines v
      machines = map parseString raw
      ans = sum (map fewestPresses machines)
   in -- in show $ head machines
      show ans

type Machine1 = ([Char], [[Int]], [Int])

parseString1 :: [String] -> Machine1
parseString1 str =
  let diags = (drop 1 $ init (head str))
      buttons = map parseBraces (drop 1 $ init str)
      jolts = parseBraces (last str)
   in (diags, buttons, jolts)

maxMult :: [Int] -> [Int] -> Int
maxMult jolts button
  | null button = 1000
  | otherwise = min (jolts !! idd) $ maxMult jolts (tail button)
  where
    idd = head button

fewestPresses1 :: Machine1 -> [Int]
fewestPresses1 (_, btns, jolts) =
  let mmult = map (maxMult jolts) btns
   in mmult

part2 :: String -> String
part2 v =
  let raw = map (splitBy (== ' ')) $ lines v
      machines = map parseString1 raw
      ans = fewestPresses1 (head machines)
   in -- in show $ head machines
      show ans

day10 :: String -> (String, String)
day10 str = (part1 str, part2 str)

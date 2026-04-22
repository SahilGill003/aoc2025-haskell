module Day2
  ( day2,
  )
where

import Data.List (isPrefixOf)

splitBy :: (Char -> Bool) -> [Char] -> [String]
splitBy isCh = foldr f [[]]
  where
    f x y =
      if isCh x
        then (if null (head y) then y else [] : y)
        else (x : head y) : tail y

toNums :: String -> (Integer, Integer)
toNums "" = (0, 0)
toNums str = (l, r)
  where
    nums = splitBy (== '-') str
    l = read $ head nums
    r = read $ nums !! 1

_isPreRep :: (String, String) -> Bool
_isPreRep (_, []) = True
_isPreRep (h, t) = (h `isPrefixOf` t) && _isPreRep (h, drop (length h) t)

isPreRep :: String -> Int -> Bool
isPreRep "" _ = False
isPreRep str times = _isPreRep $ splitAt times str

totalInvalids :: Integer -> Integer -> Integer
totalInvalids 0 0 = 0
totalInvalids x y
  | x <= y && even (length str) && isPreRep str l = x + f
  | x <= y = f
  | otherwise = 0
  where
    str = show x
    l = length str `div` 2
    f = totalInvalids (x + 1) y

part1 :: String -> String
part1 =
  show
    . sum
    . map (uncurry totalInvalids . toNums)
    . splitBy (== ',')

atleastTwice :: [Char] -> Int -> Integer
atleastTwice [] _ = 0
atleastTwice _ 0 = 0
atleastTwice str times
  | times > div len 2 = 0
  | mod len times == 0 && isPreRep str times = read str
  | otherwise = atleastTwice str $ succ times
  where
    len = length str

allInvalids :: Integer -> Integer -> Integer
allInvalids 0 0 = 0
allInvalids x y
  | x <= y = atleastTwice (show x) 1 + allInvalids (x + 1) y
  | otherwise = 0

part2 :: String -> String
part2 =
  show
    . sum
    . map (uncurry allInvalids . toNums)
    . splitBy (== ',')

day2 :: String -> (String, String)
day2 str = (part1 str, part2 str)

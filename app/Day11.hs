module Day11
  ( day11,
  )
where

import Data.Array.IArray ((!))
import qualified Data.Array.IArray as V
import qualified Data.Map as M
import qualified Data.Set as S

type AdjMat = V.Array Int [Int]

fromMaybe :: Maybe a -> a -> a
fromMaybe Nothing x = x
fromMaybe (Just x) _ = x

mlookup :: String -> M.Map String Int -> Int
mlookup str nways =
  let maybeWays = M.lookup str nways
      ways = fromMaybe maybeWays 0
   in ways

countWays :: AdjMat -> Int -> [Int] -> Int
countWays graph nsize xs =
  let source = head xs
      target = last xs
      cw = V.array (0, nsize) [(i, f i) | i <- [0 .. nsize]]
      f src
        | src == target = 1
        | null (graph ! src) = 0
        | otherwise = foldr ((+) . (!) cw) 0 (graph ! src)
   in (cw :: V.Array Int Int) ! source

compression :: [[Int]] -> Int -> AdjMat
compression intlist size =
  V.array (0, size) ([(i, []) | i <- [0 .. size]] ++ [(head x, tail x) | x <- intlist])

parse :: [String] -> [[String]]
parse str =
  let spl xs = init (head xs) : tail xs in map (spl . words) str

part1 :: String -> String
part1 v =
  let parsed = (parse . lines) v
      uniq = foldr (S.union . S.fromList) S.empty parsed
      size = S.size uniq
      toNums = map (map (`mlookup` M.fromList (zip (S.toList uniq) [0 ..])))
      graph = compression (toNums parsed) size
      paths = toNums [["you", "out"]]
   in show $ foldr ((*) . countWays graph size) 1 paths

part2 :: String -> String
part2 v =
  let parsed = (parse . lines) v
      uniq = foldr (S.union . S.fromList) S.empty parsed
      size = S.size uniq
      toNums = map (map (`mlookup` M.fromList (zip (S.toList uniq) [0 ..])))
      graph = compression (toNums parsed) size
      paths1 = toNums [["svr", "dac"], ["dac", "fft"], ["fft", "out"]]
      tpaths = [paths1, toNums [["svr", "fft"], ["fft", "dac"], ["dac", "out"]]]
      total = foldr ((+) . foldr ((*) . countWays graph size) 1) 0 tpaths 
   in show total

day11 :: String -> (String, String)
day11 str = (part1 str, part2 str)

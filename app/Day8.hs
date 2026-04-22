module Day8
  ( day8,
  )
where

import Data.List
import qualified Data.Set as Set

type Point = (Int, Int, Int)

dist3D :: Point -> Point -> Int
dist3D (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

toPoints :: [String] -> [Point]
toPoints [] = []
toPoints str = (read f :: Int, read s :: Int, read t :: Int) : toPoints (tail str)
  where
    h = head str
    (f, ft) = break (== ',') h
    remm = if ft /= "" then tail ft else ""
    (s, st) = break (== ',') remm
    t = if st /= "" then tail st else ""

allConns :: [Point] -> [(Point, Point)]
allConns [] = []
allConns points
  | length points == 1 = points'
  | otherwise = points' ++ allConns (tail points)
  where
    h = head points
    points' = map (\x -> (h,x)) (tail points)

type DSU = [Set.Set Point]

insertPoint :: Set.Set Point -> Point -> Point -> (Set.Set Point, Bool)
insertPoint set p1 p2
  | Set.member p1 set = (Set.insert p2 set, True)
  | Set.member p2 set = (Set.insert p1 set, True)
  | otherwise = (set, False)

merge :: DSU -> Point -> Point -> DSU
merge [] p1 p2 = [Set.fromList [p1, p2]]
merge dsu p1 p2
  | inserted = fset
  | otherwise = set : merge dsu' p1 p2
  where
    set : dsu' = dsu
    (set', inserted) = insertPoint set p1 p2
    dups = if inserted then filter (\x -> Set.member p1 x || Set.member p2 x) dsu' else []
    fset =
      foldl (flip Set.union) set' dups
        : filter
          ( \x ->
              not (Set.member p2 x || Set.member p1 x)
          )
          dsu'

part1 :: String -> String
part1 v =
  let points = toPoints $ lines v
      conns =
        sortBy
          ( \(a, b) (c, d) ->
              if dist3D a b <= dist3D c d
                then LT
                else GT
          )
          $ allConns points
      dsu = foldl (\d (p1, p2) -> merge d p1 p2) [] (take 1000 conns)
      compsizes = (map length . sortBy (\x y -> if length x >= length y then LT else GT)) dsu
   in show $ product $ take 3 (nub compsizes)

-- in show ldsu

singleCircuit :: [(Point, Point)] -> DSU -> Int -> Int
singleCircuit conns dsu len
  | length dsu' == 1 && length (head dsu') == len = f * s
  | otherwise = singleCircuit (tail conns) dsu' len
  where
    (p1, p2) = head conns
    dsu' = merge dsu p1 p2
    (f, _, _) = p1
    (s, _, _) = p2

part2 :: String -> String
part2 v =
  let points = toPoints $ lines v
      conns =
        sortBy
          ( \(a, b) (c, d) ->
              if dist3D a b <= dist3D c d
                then LT
                else GT
          )
          $ allConns points
   in show $ singleCircuit conns [] $ length points

day8 :: String -> (String, String)
day8 str = (part1 str, part2 str)

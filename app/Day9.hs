module Day9
  ( day9,
  )
where

import Data.List
import qualified Data.Map.Strict as M

type Point = (Int, Int)

toIndices :: [String] -> [Point]
toIndices [] = []
toIndices str = (read x :: Int, read y :: Int) : toIndices (tail str)
  where
    h = head str
    (x, remm) = break (== ',') h
    y = if remm == "" then "" else tail remm

areaRect :: Point -> Point -> Int
areaRect (x1, y1) (x2, y2) = succ (abs (x1 - x2)) * succ (abs (y1 - y2))

largestRect :: [Point] -> Int
largestRect [] = 0
largestRect indices = maximum [v | x <- indices, y <- indices, let v = areaRect x y]

part1 :: String -> String
part1 v = show $ largestRect $ toIndices $ lines v

maybePoint :: Maybe Point -> Point
maybePoint (Just p) = p
maybePoint Nothing = (maxBound :: Int, minBound :: Int)

insertm :: M.Map Int Point -> Int -> Int -> M.Map Int Point
insertm mmap k val
  | pval == (maxBound :: Int, minBound :: Int) = M.insert k (val, val) mmap
  | otherwise = M.update (\_ -> Just (min f val, max s val)) k mmap
  where
    pval@(f, s) = maybePoint $ M.lookup k mmap

toCorners :: Point -> Point -> [Point]
toCorners (x1, y1) (x2, y2) = [(x1, y1), (x2, y1), (x1, y2), (x2, y2)]

extraCorners :: Point -> Point -> [Point]
extraCorners (x1, y1) (x2, y2)
  | x1 <= x2 && y1 <= y2 = [(x, y) | x <- [x1, x2], y <- [y1 .. y2]] ++ [(x, y) | y <- [y1, y2], x <- [x1 .. x2]]
  | x1 <= x2 && y1 >= y2 = [(x, y) | x <- [x1, x2], y <- [y2 .. y1]] ++ [(x, y) | y <- [y1, y2], x <- [x1 .. x2]]
  | x1 >= x2 && y1 <= y2 = [(x, y) | x <- [x1, x2], y <- [y1 .. y2]] ++ [(x, y) | y <- [y1, y2], x <- [x2 .. x1]]
  | x1 >= x2 && y1 >= y2 = [(x, y) | x <- [x1, x2], y <- [y2 .. y1]] ++ [(x, y) | y <- [y1, y2], x <- [x2 .. x1]]

everyPair :: [Point] -> [(Point, Point)]
everyPair [] = []
everyPair points = map (\x -> (x, head points)) (tail points) ++ everyPair (tail points)

efficient :: Int -> Int -> (Int -> Int -> Int) -> Int
efficient l h fni
  | left == 0 && right == 0 = -1
  | l == m && left == 1 && right == 0 = m
  | m + 1 == h && left == 1 && right == 1 = m + 1
  -- \| m >= h = m
  | right == 1 = efficient (m + 1) h fni
  | left == 1 && right == 0 = efficient l m fni
  | otherwise = -1
  where
    m = div (h + l) 2
    left = fni l m
    right = fni (m + 1) h

fn :: (Point -> Point -> Bool) -> [(Point, Point)] -> Int -> Int -> Int
fn _ [] _ _ = 0
fn checkBounds pairs l h
  | areaRect x y < l = 0
  | flag && checkBounds x y = 1
  | otherwise = fn checkBounds (tail pairs) l h
  where
    (x, y) = head pairs
    flag = areaRect x y >= l && areaRect x y <= h

part2 :: String -> String
part2 v =
  let bounds = toIndices $ lines v
      rows = foldl (\m (x, y) -> insertm m y x) M.empty bounds
      cols = foldl (\m (x, y) -> insertm m x y) M.empty bounds
      cmap = M.foldlWithKey (\m k (f, s) -> foldl (\m' x -> insertm m' x k) m [f .. s]) cols rows
      rmap = M.foldlWithKey (\m k (f, s) -> foldl (\m' x -> insertm m' x k) m [f .. s]) rows cols
      allPossible = sortBy (\(x1, y1) (x2, y2) -> if areaRect x1 y1 <= areaRect x2 y2 then GT else LT) $ everyPair bounds
      withinBounds (x1, y1) =
        let (lx, rx) = maybePoint $ M.lookup y1 rmap
            (ly, ry) = maybePoint $ M.lookup x1 cmap
         in (lx <= x1 && rx >= x1) && (ly <= y1 && ry >= y1)
      checkBounds x y = all withinBounds $ extraCorners x y
      allPossible1 = [(x, y) | (x, y) <- allPossible, all withinBounds $ toCorners x y, areaRect x y > 1000000000]

      val = efficient 0 100000000000 (fn checkBounds allPossible1)
   in show val

-- Answer = 1624057680

day9 :: String -> (String, String)
day9 str = (part1 str, part2 str)

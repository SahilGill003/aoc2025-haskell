module Day5
  ( day5,
  )
where

import qualified Data.Set as Set

freshIds :: [(Int, Int)] -> [Int] -> Int
freshIds [] _ = 0
freshIds _ [] = 0
freshIds ranges keys
  | isFresh = 1 + freshIds ranges rkeys
  | otherwise = freshIds ranges rkeys
  where
    k = head keys
    rkeys = tail keys
    isFresh = any (\(x, y) -> k >= x && k <= y) ranges

parseIngredients :: [String] -> ([(Int, Int)], [Int])
parseIngredients gs
  | null extra = (numRanges, [])
  | otherwise = (numRanges, numIds)
  where
    (strRanges, extra) = break (== "") gs
    numRanges =
      map
        ( \x ->
            let (a, b) = break (== '-') x
             in (read a :: Int, read (tail b) :: Int)
        )
        strRanges
    numIds = map (\x -> read x :: Int) (tail extra)

part1 :: String -> String
part1 v =
  let ls = lines v
      (numRanges, numIds) = parseIngredients ls
   in show $ freshIds numRanges numIds

disjoint :: (Int, Int) -> (Int, Int) -> (Int, Int)
disjoint lr ab
  | a > r || l > b = lr
  | l <= a && b <= r = lr
  | l <= a && b >= r = (l, b)
  | a <= l && r <= b = ab
  | a <= l && r >= b = (a, r)
  | otherwise = lr
  where
    (l, r) = lr
    (a, b) = ab

keepDisjoint :: [(Int, Int)] -> [(Int, Int)]
keepDisjoint [] = []
keepDisjoint initial
  | optimal = sett
  | otherwise = keepDisjoint sett
  where
    fset list = Set.fromList $ map (\i -> foldl disjoint i list) list
    sett = Set.toList $ fset $ Set.toList $ fset initial
    optimal = length sett == length initial

part2 :: String -> String
part2 v =
  let ls = lines v
      (numRanges, _) = parseIngredients ls
   in show $ foldl (\x y -> let (a, b) = y in x + b - a + 1) 0 (keepDisjoint numRanges)

day5 :: String -> (String, String)
day5 str = (part1 str, part2 str)

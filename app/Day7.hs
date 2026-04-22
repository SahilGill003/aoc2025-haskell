module Day7
  ( day7,
  )
where

import qualified Data.Map as M

findIndex :: (Eq a) => [a] -> a -> Int
findIndex [] _ = minBound :: Int
findIndex (h : t) x = if h == x then 0 else 1 + findIndex t x

contains :: (Eq a) => [a] -> a -> Bool
contains [] _ = False
contains (h : t) x = h == x || contains t x

keepFinding :: [Char] -> Char -> Int -> [Int]
keepFinding [] _ _ = []
keepFinding str key idx
  | head str == key = idx : keepFinding (tail str) key (succ idx)
  | otherwise = keepFinding (tail str) key $ succ idx

toIndices :: [String] -> [[Int]]
toIndices [] = []
toIndices str = projector : splitters
  where
    projector = [findIndex (head str) 'S']
    splitters = map (\x -> let y = keepFinding x '^' 0 in y) $ tail str

splitBeam :: Int -> Int -> Int -> M.Map Int Int -> M.Map Int Int
splitBeam col prev n mmap =
  let (lcol, rcol) = (col - 1, col + 1)
      xmap = M.update (const Nothing) col mmap
      lup x = M.lookup x xmap
      lmap =
        if lcol < 0
          then xmap
          else case lup lcol of
            Nothing -> M.insert lcol prev xmap
            _ -> M.update (\curr -> Just (curr + prev)) lcol xmap
      rmap =
        if rcol >= n
          then lmap
          else case lup rcol of
            Nothing -> M.insert rcol prev lmap
            _ -> M.update (\curr -> Just (curr + prev)) rcol lmap
   in rmap

_topToBottom :: [[Int]] -> Int -> M.Map Int Int -> (Int, Int)
_topToBottom [] _ _ = (0, 0)
_topToBottom splitters n mmap
  | length splitters == 1 = (splits, sum nmap)
  | otherwise = (splits, timelines)
  where
    mmaplist = M.toList mmap
    top = head splitters
    found =
      filter (/= (-1, -1)) $
        map
          ( \(k, v) ->
              if contains top k
                then (k, v)
                else (-1, -1)
          )
          mmaplist
    nmap = foldl (\mp (col, val) -> splitBeam col val n mp) mmap found
    nxt = _topToBottom (tail splitters) n nmap
    splits = length found + fst nxt
    timelines = snd nxt

topToBottom :: [[Int]] -> Int -> (Int, Int)
topToBottom splitters n =
  let beams = M.fromList $ zip (head splitters) [1]
      nsplitters = tail splitters
   in _topToBottom nsplitters n beams

combinePart :: String -> (String, String)
combinePart v =
  let ls = lines v
      n = length (head ls)
      splitters = toIndices ls
      (f, s) = topToBottom splitters n
   in (show f, show s)

day7 :: String -> (String, String)
day7 = combinePart

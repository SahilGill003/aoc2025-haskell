module Day4
  ( day4,
  )
where

cnt :: [String] -> Int
cnt = sum . map (length . filter (== '@'))

dfs :: [String] -> [Char]
dfs [] = []
dfs mat = _dfs mat 0 0

_dfs :: [String] -> Int -> Int -> [Char]
_dfs [] _ _ = []
_dfs mat i j
  | i >= m = []
  | j >= n = '\n' : _dfs mat (succ i) 0
  | mat !! i !! j /= '@' = '.' : nxt
  | otherwise = final
  where
    m = length mat
    n = length $ head mat
    check x y = x >= 0 && y >= 0 && x < m && y < n
    checkRollable :: Int -> Int -> Int
    checkRollable x y = if check x y && mat !! x !! y == '@' then 1 else 0
    dirs = [0, 1, -1]
    counts = [checkRollable (i + x) (j + y) | x <- dirs, y <- dirs]
    nxt = _dfs mat i $ succ j
    final = (if sum counts <= 4 then '.' else '@') : nxt

dodfs :: [String] -> [String] -> Int
dodfs [] _ = 0
dodfs mat dup
  | dup /= nxt = 1 + dodfs mat nxt
  | otherwise = 0
  where
    nxt = lines $ dfs dup

part1 :: String -> String
part1 v = show $ cnt (lines v) - cnt (lines $ dfs (lines v))

part2 :: String -> String
part2 v = show $ dodfs (lines v) (lines v)

day4 :: String -> (String, String)
day4 str = (part1 str, part2 str)

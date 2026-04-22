module Day1
( day1
) where 

parseInt :: [Char] -> Int
parseInt ('L' : r)  = read r
parseInt ('R' : r)  = -read r
parseInt _ = 0

spinCheck :: Int -> [Int] -> Int
spinCheck _ [] = 0
spinCheck prev vec
    | curr == 0 = 1 + f
    | otherwise = f 
    where h = head vec
          t = tail vec
          curr = (prev + h + 100 * abs (div h 100)) `mod` 100
          f = spinCheck curr t

countZeros :: Int -> Int -> Int
countZeros curr prev 
  | curr >= 0 = div (curr + prev) 100 
  | mod (abs curr) 100 >= prev && prev /= 0 = 1 + div (abs curr) 100
  | mod (abs curr) 100 < prev || prev == 0 = div (abs curr) 100
  | otherwise = 0

interSpinCheck :: Int -> [Int] -> Int
interSpinCheck _ [] = 0 
interSpinCheck prev vec = zeros + f
     where
         h = head vec
         t = tail vec
         fr = abs $ div h 100
         zeros = countZeros h prev
         curr = (h + prev + 100 * (1 + fr)) `mod` 100
         f = interSpinCheck curr t

part1 :: String -> String
part1 = show
          . spinCheck 50 
          . map parseInt . lines 

part2 :: String -> String
part2 = show 
          . interSpinCheck 50
          . map parseInt . lines 

day1 :: String -> (String, String)
day1 str = (part1 str, part2 str)

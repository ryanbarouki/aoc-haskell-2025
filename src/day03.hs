import AOC

main :: IO ()
main = interact solve

solve :: String -> String
solve input =
  let result1 = part1 input
      result2 = part2 input
   in unlines
        [ "Part 1: " ++ result1,
          "Part 2: " ++ result2
        ]

part1 :: String -> String
part1 = show . sum . map (largestNumOfLen 2) . lines

part2 :: String -> String
part2 = show . sum . map (largestNumOfLen 12) . lines

largestNumOfLen n s = read ans
  where
    ans = (reverse . snd) $ foldl foldf (-1, "") [n, n - 1 .. 1]
    foldf (from, acc) fromEnd = (nextFrom, nextNum : acc)
      where
        to = length s - fromEnd
        (nextFrom, nextNum) = largestNumInSlice (from + 1) to s

largestNumInSlice from to s = (idx, s !! idx)
  where
    idx = from + largestNunIdx sliced
    sliced = slice from to s

largestNunIdx :: String -> Int
largestNunIdx s = ans
  where
    n = head $ dropWhile (\nc -> isNothing (lookFor nc s)) "987654321"
    ans = case lookFor n s of
      Just idx -> idx
      Nothing -> -1

lookFor :: Char -> String -> Maybe Int
lookFor n [] = Nothing
lookFor nc' str = go nc' str 0
  where
    go nc [] n = Nothing
    go nc (c : s) n = if c == nc then Just n else go nc s (n + 1)

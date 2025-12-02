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

parseInput :: String -> [Int]
parseInput input = map (\(x : xs) -> let n = read xs in if x == 'R' then n else -n) (lines input)

part1 :: String -> String
part1 input = show $ length $ filter (== 0) $ foldl foldf [50] nums
  where
    foldf acc x = (head acc + x) `mod` 100 : acc
    nums = parseInput input

part2 :: String -> String
part2 input = show total
  where
    moves = parseInput input

    (_, total) = foldl step (50, 0) moves

    step (pos, acc) x =
      let crosses
            | x >= 0 = (pos + x) `div` 100 - pos `div` 100
            | otherwise = ((pos + x + 99) `div` 100) - ((pos + 99) `div` 100)
          newPos = (pos + x) `mod` 100
          wrapped = if newPos < 0 then newPos + 100 else newPos
       in (wrapped, acc + abs crosses)

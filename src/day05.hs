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

parse' :: String -> ([(Integer, Integer)], [Integer])
parse' input = (ranges, ids)
  where
    ranges = map (\[i, j] -> (i, j)) $ filter ((> 1) . length) numList
    ids = concat $ filter ((== 1) . length) numList
    numList = map numbers' (lines input)

solve1 :: [(Integer, Integer)] -> [Integer] -> Int
solve1 ranges ids = length fresh
  where
    fresh = filter isFresh ids
    isFresh id = any (\(low, high) -> id >= low && id <= high) ranges

part1 :: String -> String
part1 input = show $ solve1 ranges ids
  where
    (ranges, ids) = parse' input

solve2 :: [(Integer, Integer)] -> Integer
solve2 ranges = sum rangeDiffs
  where
    rangeDiffs = map (\(a, b) -> (b - a) + 1) intervals
    intervals = foldl (\acc r -> init acc ++ rangeUnion (last acc) r) [minimum ranges] (sort ranges)

part2 :: String -> String
part2 input = show $ solve2 ranges
  where
    (ranges, _) = parse' input

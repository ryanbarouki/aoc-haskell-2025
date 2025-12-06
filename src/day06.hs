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
part1 input = show $ sum $ map (uncurry doOp) zipped
  where
    zipped = zip (ops input) (nums input)
    nums = transpose . map numbers' . init . lines
    ops = words . last . lines

part2 :: String -> String
part2 input = show $ sum $ map (uncurry doOp) zipped
  where
    zipped = zip (ops input) (nums input)
    nums = map concat . splitWhen (== []) . map numbers' . transpose . init . lines
    ops = words . last . lines

doOp :: String -> [Int] -> Int
doOp op xs
  | op == "+" = sum xs
  | op == "*" = product xs

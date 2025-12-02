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
part1 input = show $ sum $ concatMap (invalidIdsInRange invalidId) (pairup ns)
  where
    ns = map show (numbers' input)
    pairup [] = []
    pairup (x : y : xs) = (x, y) : pairup xs

invalidIdsInRange p (s, e) = filter p [sn .. en]
  where
    sn = read s :: Int
    en = read e :: Int

invalidId id
  | odd len = False
  | otherwise = first == second
  where
    idstr = show id
    len = length idstr
    hlen = len `div` 2
    first = take hlen idstr
    second = drop hlen idstr

part2 :: String -> String
part2 input = show $ sum $ concatMap (invalidIdsInRange invalidId2) (pairup ns)
  where
    ns = map show (numbers' input)
    pairup [] = []
    pairup (x : y : xs) = (x, y) : pairup xs

invalidIdn :: Int -> String -> Bool
invalidIdn n id = all (uncurry (==)) $ zip (repeat first) (chunk n id)
  where
    first = take n id

invalidId2 id = any (\n -> invalidIdn n idstr) [1 .. hlen]
  where
    idstr = show id
    hlen = length idstr `div` 2

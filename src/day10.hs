import AOC
import Data.Bits
import Data.Set qualified as S

main :: IO ()
main = do
  input <- getContents
  (result1, t1) <- timeCpu . evaluate . force . part1 $ input
  putStrLn $ "Part 1: " ++ result1 ++ " | time: " ++ show t1 ++ " s"
  (result2, t2) <- timeCpu . evaluate . force . part2 $ input
  putStrLn $ "Part 2: " ++ result2 ++ " | time: " ++ show t2 ++ " s"

type Lights = Int -- Binary

type Buttons = [Int] -- Binary

type Joltages = [Int]

-- [.#...#] (2,4) (3,4) (0,2,3,5) (0,1,2,3) (1,3,4,5) {26,32,28,47,23,20}
-- parser :: Parser Lights
-- parser = do
--   char '['
--   lightStr <- some (char '.' <|> char '#')
--   let lights = map (== '#') lightStr
--   char ']'
--   space
--   char '('
--   button <- sepBy (some digitChar) (char ',')
--   return $ binToDec lights

part1 :: String -> String
-- part1 input = show $ map (parse parser) (lines input)
part1 input = show $ sum $ map p (lines input)
  where
    -- p :: String -> (Lights, Buttons)
    p l = solve1 lights buttonsAsBinary
      where
        split = splitOn " " l
        lights :: Int = binToDec $ reverse $ map (== '#') (init $ tail $ head split)
        joltages = last split

        buttons = map (\l -> head $ map numbers' (splitOn ")" l)) (init $ tail split)
        buttonsAsBinary = map listToBin buttons
        listToBin [] = 0
        listToBin (b : bs) = (1 `shiftL` b) .|. listToBin bs :: Int

solve1 final bs = bfs (neighbours bs) (== final) S.empty [(0, 0)]

bfs :: (Ord a) => (a -> [a]) -> (a -> Bool) -> S.Set a -> [(a, Int)] -> Int
bfs neighbours end finished ((f, acc) : frontier)
  | end f = acc
  | otherwise = bfs neighbours end (S.insert f finished) (frontier ++ filtered)
  where
    filtered = zip (filter (\n -> n `S.notMember` finished && n `notElem` map fst frontier) $ neighbours f) (repeat (acc + 1))

-- bfs _ _ acc _ [] = acc

neighbours :: Buttons -> Lights -> [Lights]
neighbours bs l = map (`xor` l) bs

-- solve1 :: (Lights, Buttons)

part2 :: String -> String
part2 input = show $ sum $ map p ([head $ lines input])
  where
    -- p :: String -> (Lights, Buttons)
    p l = solve2 joltages buttons
      where
        split = splitOn " " l
        lights :: Int = binToDec $ reverse $ map (== '#') (init $ tail $ head split)
        joltages = numbers' $ last split

        buttons = map (\l -> head $ map numbers' (splitOn ")" l)) (init $ tail split)

solve2 finalJoltages bs = bfs (neighbours2 bs finalJoltages) (== finalJoltages) S.empty [(replicate (length finalJoltages) 0, 0)]

-- neighbours2 :: [[Int]] -> Joltages -> [Joltages]
neighbours2 bs finalJoltages js = filteredJoltages
  where
    filteredJoltages = filter (\njs -> all (\(finalJ, j) -> j <= finalJ) (zip finalJoltages njs)) nextJoltages
    nextJoltages = map (press js) bs
    press :: Joltages -> [Int] -> Joltages
    press js b = map (\(i, jolt) -> if i `elem` b then jolt + 1 else jolt) (zip [0 ..] js) :: Joltages

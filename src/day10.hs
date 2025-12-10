import AOC
import Data.Bits
import Data.SBV
import Data.Set qualified as S

main :: IO ()
main = do
  input <- getContents
  (result1, t1) <- timeCpu . evaluate . force . part1 $ input
  putStrLn $ "Part 1: " ++ result1 ++ " | time: " ++ show t1 ++ " s"

  (result2, t2) <- timeCpu $ part2 input
  putStrLn $ "Part 2: " ++ show result2 ++ " | time: " ++ show t2 ++ " s"

type Lights = Int -- Binary

type Buttons = [Int] -- Binary

type Joltages = [Int]

part1 :: String -> String
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

neighbours :: Buttons -> Lights -> [Lights]
neighbours bs l = map (`xor` l) bs

-- PART 2
problem nButtons constraints = do
  ns <- sequence $ map sInteger ["n" ++ show i | i <- [0 .. nButtons - 1]]

  mapM_ constrain [n .>= 0 | n <- ns]

  mapM_ (\(j, bIndices) -> constrain $ (sum $ map (\i -> ns !! i) bIndices) .== literal (fromIntegral j)) constraints

  minimize "steps" $ sum ns

processLine :: String -> IO Int
processLine l = do
  let splitLine = splitOn " " l

  let buttonsStr = init $ tail splitLine
      joltagesStr = last splitLine

      buttonMap :: [[Int]]
      buttonMap = map (\s -> numbers' (head $ splitOn ")" s)) buttonsStr

      nButtons = length buttonMap

      joltages :: [Int] = numbers' joltagesStr

      buttonIndices valIdx = filter (\i -> valIdx `elem` (buttonMap !! i)) [0 .. nButtons - 1]

      constraints = zip joltages [buttonIndices valIdx | valIdx <- [0 .. length joltages - 1]]

  let problemGoal = problem nButtons constraints

  result <- optimize Lexicographic problemGoal
  let strResult = show result
  return $ head $ numbers' $ last (lines strResult)

part2 :: String -> IO Int
part2 input = do
  foldM (\acc l -> processLine l >>= (\i -> return $ acc + i)) 0 (lines input)

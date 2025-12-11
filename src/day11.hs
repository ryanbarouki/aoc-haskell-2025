import AOC
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as S

type Graph = HM.HashMap String (S.HashSet String)

type Path = S.HashSet String

main :: IO ()
main = do
  input <- getContents
  (result1, t1) <- timeCpu . evaluate . force . part1 $ input
  putStrLn $ "Part 1: " ++ result1 ++ " | time: " ++ show t1 ++ " s"

  (result2, t2) <- timeCpu . evaluate . force . part2 $ input
  putStrLn $ "Part 2: " ++ result2 ++ " | time: " ++ show t2 ++ " s"

part1 :: String -> String
part1 input = show $ countPaths graph "you"
  where
    graph = HM.fromList $ map parser (lines input)
    parser line = (init $ head $ words line, S.fromList $ tail $ words line)

countPaths :: Graph -> String -> Int
countPaths g start = go S.empty start
  where
    go path "out" = 1
    go path curr = sum $ map (go (curr `S.insert` path)) $ S.toList nextPoints
      where
        nextPoints = S.filter (\n -> not $ n `S.member` path) (g HM.! curr)

part2 :: String -> String
part2 input = show $ countPaths2 graph "svr"
  where
    graph = HM.fromList $ map parser (lines input)
    parser line = (init $ head $ words line, S.fromList $ tail $ words line)

countPaths2 :: Graph -> String -> Int
countPaths2 g start = memogo S.empty (False, False) start
  where
    memogo = memo3' go
    go path (seenDAC, seenFFT) "out" = if seenDAC && seenFFT then 1 else 0
    go path (seenDAC, seenFFT) curr = sum $ map (memogo (curr `S.insert` path) (seenDAC || (curr == "dac"), seenFFT || (curr == "fft"))) $ S.toList nextPoints
      where
        nextPoints = S.filter (\n -> not $ n `S.member` path) (g HM.! curr)

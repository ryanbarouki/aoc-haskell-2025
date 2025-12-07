import AOC
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as S

type Grid = HM.HashMap (Int, Int) Char

type PathCount = HM.HashMap (Int, Int) Int

type Point = (Int, Int)

type Frontier = S.Set Point

main :: IO ()
main = do
  input <- getContents
  -- time part1
  (result1, t1) <- timeCpu . evaluate . force . part1 $ input
  -- time part2
  (result2, t2) <- timeCpu . evaluate . force . part2 $ input
  -- normal output
  putStrLn $ "Part 1: " ++ result1 ++ " | time: " ++ show t1 ++ " s"
  putStrLn $ "Part 2: " ++ result2 ++ " | time: " ++ show t2 ++ " s"

fst' (x, _, _) = x

snd' (_, x, _) = x

thd' (_, _, x) = x

advanceFrontier :: Frontier -> Frontier
advanceFrontier = S.map (\(x, y) -> (x, y + 1))

part1 :: String -> String
part1 input = show . thd' . last . takeWhile (not . done . snd') $ iterate step (g, frontier, 0)
  where
    g = enumerateHM input :: Grid
    frontier = S.fromList $ filter (\p -> g HM.! p == 'S') (HM.keys g)
    maxY = maximum $ map snd (HM.keys g)
    done = (> maxY - 1) . snd . S.findMin

step :: (Grid, Frontier, Int) -> (Grid, Frontier, Int)
step (grid, frontier, count) = (grid, frontier', count + count')
  where
    (frontier', count') = stepFrontier frontier
    stepFrontier = updateFrontier . advanceFrontier
    updateFrontier = foldl foldf (S.empty, 0)
    foldf (acc, c) p@(x, y)
      | grid HM.! p == '^' = (S.insert (x + 1, y) $ S.insert (x - 1, y) acc, c + 1)
      | otherwise = (S.insert p acc, c)

part2 :: String -> String
part2 input = show $ foldl (\acc p -> acc + pc' HM.! p) 0 finalFrontier
  where
    (finalFrontier, pc') = (\(_, f, c) -> (f, c)) . last . takeWhile (not . done . snd') $ iterate step2 (g, frontier, pathCount)
    g = enumerateHM input :: Grid
    start = head $ filter (\p -> g HM.! p == 'S') (HM.keys g)
    frontier = S.fromList [start]
    pathCount = HM.insert start 1 $ HM.map (const 0) g
    maxY = maximum $ map snd (HM.keys g)
    done = (> maxY - 1) . snd . S.findMin

step2 :: (Grid, Frontier, PathCount) -> (Grid, Frontier, PathCount)
step2 (grid, frontier, pathCount) = (grid, frontier', pc')
  where
    (pc', frontier') = stepFrontier frontier
    stepFrontier = updateFrontier . advanceFrontier
    updateFrontier = foldl foldf (pathCount, S.empty)

    foldf (pc, acc) p@(x, y) = case grid HM.! p of
      '^' ->
        ( addPathCount (x + 1, y) oldCount $ addPathCount (x - 1, y) oldCount pc,
          S.fromList [(x - 1, y), (x + 1, y)] <> acc
        )
      '.' -> (addPathCount p oldCount pc, S.insert p acc)
      where
        oldCount = pc HM.! (x, y - 1)

    addPathCount p val = HM.adjust (+ val) p

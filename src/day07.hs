import AOC
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as S

type Grid = HM.HashMap (Int, Int) Char

type PathCount = HM.HashMap (Int, Int) Int

type Point = (Int, Int)

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

advanceFrontier :: [Point] -> [Point]
advanceFrontier = map (\(x, y) -> (x, y + 1))

updateFrontier :: Grid -> [Point] -> (S.Set Point, Int)
updateFrontier grid = foldl foldf (S.empty, 0)
  where
    foldf (acc, c) p@(x, y)
      | grid HM.! p == '^' =
          ( S.insert (x + 1, y) (S.insert (x - 1, y) acc),
            c + 1
          )
      | otherwise = (S.insert p acc, c)

part1 :: String -> String
part1 input = show $ (\(_, _, c, _) -> c) . last $ takeWhile (\(_, _, _, done) -> not done) $ iterate step (g, frontier, 0, False)
  where
    g = enumerateHM input :: Grid
    frontier = filter (\p -> g HM.! p == 'S') (HM.keys g)

step :: (Grid, [Point], Int, Bool) -> (Grid, [Point], Int, Bool)
step (grid, frontier, count, _)
  | done (advanceFrontier frontier) = (grid, frontier, count, True)
  | otherwise = (grid, frontier', count + count', False)
  where
    (frontier', count') = (\(a, b) -> (S.toList a, b)) . stepFrontier $ frontier
    stepFrontier = updateFrontier . advanceFrontier
    updateFrontier = foldl foldf (S.empty, 0)

    foldf (acc, c) p@(x, y) =
      if grid HM.! p == '^'
        then
          ( S.insert (x + 1, y) $
              S.insert (x - 1, y) acc,
            c + 1
          )
        else (S.insert p acc, c)

    maxY = maximum $ map snd (HM.keys grid)
    done = any (\(x, y) -> y > maxY)

part2 :: String -> String
part2 input = show $ foldl (\acc p -> acc + pc' HM.! p) 0 finalFrontier
  where
    (pc', finalFrontier) = (\(_, f, c, _) -> (c, f)) . last $ takeWhile (\(_, _, _, done) -> not done) $ iterate step2 (g, frontier, pathCount, False)
    g = enumerateHM input :: Grid
    frontier = filter (\p -> g HM.! p == 'S') (HM.keys g)
    pathCount = HM.insert (head frontier) 1 $ foldl (\acc p -> HM.insert p 0 acc) HM.empty (HM.keys g)

step2 :: (Grid, [Point], PathCount, Bool) -> (Grid, [Point], PathCount, Bool)
step2 (grid, frontier, pathCount, _)
  | done (advanceFrontier frontier) = (grid, frontier, pathCount, True)
  | otherwise = (grid, frontier', pc', False)
  where
    (pc', frontier') = (\(a, b) -> (a, S.toList b)) . stepFrontier $ frontier
    stepFrontier = updateFrontier . advanceFrontier
    updateFrontier = foldl foldf (pathCount, S.empty)

    foldf (pc, acc) p@(x, y) = case grid HM.! p of
      '^' ->
        ( addPathCount (x + 1, y) oldCount $
            addPathCount (x - 1, y) oldCount pc,
          S.insert (x + 1, y) $
            S.insert (x - 1, y) acc
        )
      '.' -> (addPathCount p oldCount pc, S.insert p acc)
      where
        oldCount = pc HM.! (x, y - 1)

    addPathCount p val pc = HM.insert p (pc HM.! p + val) pc

    maxY = maximum $ map snd (HM.keys grid)
    done = any (\(x, y) -> y > maxY)

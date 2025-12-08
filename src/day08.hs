import AOC
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as S

main :: IO ()
main = do
  input <- getContents
  (result1, t1) <- timeCpu . evaluate . force . part1 $ input
  putStrLn $ "Part 1: " ++ result1 ++ " | time: " ++ show t1 ++ " s"
  (result2, t2) <- timeCpu . evaluate . force . part2 $ input
  putStrLn $ "Part 2: " ++ result2 ++ " | time: " ++ show t2 ++ " s"

part1 :: String -> String
part1 input = show $ product $ take 3 $ reverse . sort $ map S.size $ nubOrd $ HM.elems $ solve1 0 pairs clusterByPoint
  where
    points = map ((\[x, y, z] -> Point x y z) . numbers') (lines input)
    pairs = sortOn (\[p, p'] -> euclidean p p') $ choose 2 points
    clusterByPoint = foldl (\acc p -> HM.insert p (S.fromList [p]) acc) (HM.empty :: ClusterMap) points

part2 :: String -> String
part2 input = show $ (\[Point x _ _, Point x' _ _] -> x * x') $ solve2 0 pairs clusterByPoint
  where
    points = map ((\[x, y, z] -> Point x y z) . numbers') (lines input)
    pairs = sortOn (\[p, p'] -> euclidean p p') $ choose 2 points
    clusterByPoint = foldl (\acc p -> HM.insert p (S.fromList [p]) acc) (HM.empty :: ClusterMap) points

solve1 :: Int -> [[Point]] -> ClusterMap -> ClusterMap
solve1 _ [] clusterByPoint = clusterByPoint
solve1 1000 _ clusterByPoint = clusterByPoint
solve1 n pairs clusterByPoint = solve1 (n + 1) pairs clusterByPoint'
  where
    [p, p'] = pairs !! n
    newGroup = clusterByPoint HM.! p <> clusterByPoint HM.! p'
    clusterByPoint' = S.foldl (\m q -> HM.insert q newGroup m) clusterByPoint newGroup

solve2 :: Int -> [[Point]] -> ClusterMap -> [Point]
solve2 _ [] _ = []
solve2 n pairs clusterByPoint
  | S.size (clusterByPoint' HM.! p) == 1000 = pairs !! n
  | otherwise = solve2 (n + 1) pairs clusterByPoint'
  where
    [p, p'] = pairs !! n
    newGroup = clusterByPoint HM.! p <> clusterByPoint HM.! p'
    clusterByPoint' = S.foldl (\m q -> HM.insert q newGroup m) clusterByPoint newGroup

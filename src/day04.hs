import AOC
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as S

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

neighbours p gridHM = filter (`HM.member` gridHM) (neighbours8 p)

step (_, gridHM) = (c', newGridHM)
  where
    newGridHM = foldl foldf HM.empty (HM.keys gridHM)
    c' = length toRemove
    toRemove = filter (\p -> length (neighbourRolls p) < 4) rollPoints
    toRemoveSet = S.fromList toRemove
    rollPoints = filter (\p -> gridHM HM.! p == '@') (HM.keys gridHM)
    neighbourRolls p = filter (\p' -> gridHM HM.! p' == '@') (neighbours p gridHM)

    foldf newGrid p = if p `S.member` toRemoveSet then HM.insert p '.' newGrid else HM.insert p (gridHM HM.! p) newGrid

part1 :: String -> String
part1 input = show . fst . last . take 2 $ iterate step (0, gridHM)
  where
    gridHM = enumerateHM input :: HM.HashMap (Int, Int) Char

part2 :: String -> String
part2 input = show . sum . map fst . takeWhile ((/= 0) . fst) . tail $ iterate step (0, gridHM)
  where
    gridHM = enumerateHM input :: HM.HashMap (Int, Int) Char

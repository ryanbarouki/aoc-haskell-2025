import AOC
import Data.Set qualified as S

main :: IO ()
main = do
  input <- getContents
  (result1, t1) <- timeCpu . evaluate . force . part1 $ input
  putStrLn $ "Part 1: " ++ result1 ++ " | time: " ++ show t1 ++ " s"
  (result2, t2) <- timeCpu . evaluate . force . part2 $ input
  putStrLn $ "Part 2: " ++ result2 ++ " | time: " ++ show t2 ++ " s"

-- This feels very dirty indeed. I did NOT expect this to be the right answer,
-- only an upper bound, but it appears to be! (I feel dirty).
part1 :: String -> String
part1 input = show $ length $ filter (\(sizes, copies) -> not $ tooManyPoints shapes copies sizes) (zip allSizes allCopies)
  where
    [ss0, ss1, ss2, ss3, ss4, ss5, packing] = splitOn "\n\n" input
    shapes = map (enumerateFilterSet (== '#') . tail . snd . breakOn "\n") [ss0, ss1, ss2, ss3, ss4, ss5]

    packingNumbers = map numbers' (lines packing)
    allSizes = map (\(w : h : _) -> (w, h)) packingNumbers
    allCopies = map (\(_ : _ : cps) -> cps) packingNumbers

tooManyPoints shapes copies (h, w) = sum (map (\(s, c) -> c * S.size s) (zip shapes copies)) > h * w

part2 :: String -> String
part2 input = "Merry Christmas!"

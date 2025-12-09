import AOC
import Data.Set qualified as S

main :: IO ()
main = do
  input <- getContents
  (result1, t1) <- timeCpu . evaluate . force . part1 $ input
  putStrLn $ "Part 1: " ++ result1 ++ " | time: " ++ show t1 ++ " s"
  (result2, t2) <- timeCpu . evaluate . force . part2 $ input
  putStrLn $ "Part 2: " ++ result2 ++ " | time: " ++ show t2 ++ " s"

part1 :: String -> String
part1 input = show $ solve1 points
  where
    points = map numbers' (lines input)

area [[x, y], [x', y']] = (abs (x' - x) + 1) * (abs (y' - y) + 1)

solve1 :: [[Int]] -> Int
solve1 points = maximum areas
  where
    areas = map area pairs
    pairs = choose 2 points

getBorder [] = S.empty
getBorder [[x, y]] = S.empty
getBorder ([x, y] : [x', y'] : ps)
  | x == x' = S.fromList [(x, y'') | y'' <- [minY .. maxY]] <> getBorder ([x', y'] : ps)
  | y == y' = S.fromList [(x'', y) | x'' <- [minX .. maxX]] <> getBorder ([x', y'] : ps)
  | otherwise = error "What!?"
  where
    xs = [x, x']
    ys = [y, y']
    maxX = maximum xs
    minX = minimum xs
    maxY = maximum ys
    minY = minimum ys

-- ray cast beam off to the right
pointInside p poly = odd $ countIntersect p poly

-- tail recursive versions
countIntersect p poly = go 0 poly
  where
    go n [p] = n
    go count ([x1, y1] : [x2, y2] : ps)
      | (y1 > y') /= (y2 > y') && x <= x1 = go (count + 1) ([x2, y2] : ps)
      | otherwise = go count ([x2, y2] : ps)
    [x, y] = p
    y' = y + 0.5

edgeIntersect edge poly = go False poly
  where
    go acc [p] = acc
    go acc ([x1, y1] : [x2, y2] : ps)
      | y == y' && x1 == x2 = go (((y1 > y) /= (y2 > y) && (x > x1) /= (x' > x1)) || acc) ([x2, y2] : ps)
      | x == x' && y1 == y2 = go (((x1 > x) /= (x2 > x) && (y > y1) /= (y' > y1)) || acc) ([x2, y2] : ps)
      | otherwise = go acc ([x2, y2] : ps)
    [[x, y], [x', y']] = edge

inscribeRect [[x1, y1], [x2, y2], [x3, y3], [x4, y4]] = [[x1 + 0.5, y1 + 0.5], [x2 + 0.5, y2 - 0.5], [x3 - 0.5, y3 + 0.5], [x4 - 0.5, y4 - 0.5]]

-- assuming a sorted order
getEdges [p1, p2, p3, p4] = [[p1, p2], [p1, p3], [p2, p4], [p3, p4]]

-- sorts the rectangle points into a canonical order (y increases downward)
-- this makes the inscribeRect funciton much easier to write
rect [[x1, y1], [x2, y2]] = sort [[x1, y1], [x2, y2], [x1, y2], [x2, y1]]

part2 :: String -> String
part2 input = show (floor $ maximum areas :: Int)
  where
    filterPairs = filter filterFunction pairs
    filterFunction pair@[[x1, y1], [x2, y2]]
      -- Ignoring the lines as they are likely not the largest area
      | x1 == x2 || y1 == y2 = False
      | otherwise =
          ((x1, y2) `S.member` border || pointInside [x1, y2] poly)
            && ((x2, y1) `S.member` border || pointInside [x2, y1] poly)
            && all (\edge -> not $ edgeIntersect edge poly) (getEdges . inscribeRect $ rect pair)

    areas = map area filterPairs
    pairs = choose 2 points
    points = map numbers' (lines input)
    border = getBorder (points ++ [head points])
    poly = points ++ [head points]

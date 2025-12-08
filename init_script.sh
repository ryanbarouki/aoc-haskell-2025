#!/usr/bin/env bash

for i in $(seq -w 01 12); do
  cat > "src/day$i.hs" <<EOF
import AOC

main :: IO ()
main = do
  input <- getContents
  (result1, t1) <- timeCpu . evaluate . force . part1 $ input
  putStrLn $ "Part 1: " ++ result1 ++ " | time: " ++ show t1 ++ " s"
  (result2, t2) <- timeCpu . evaluate . force . part2 $ input
  putStrLn $ "Part 2: " ++ result2 ++ " | time: " ++ show t2 ++ " s"

part1 :: String -> String
part1 input = "not implemented"

part2 :: String -> String
part2 input = "not implemented"
EOF
done

#!/usr/bin/env bash

for i in $(seq -w 01 12); do
  cat > "src/day$i.hs" <<EOF
import AOC

main :: IO ()
main = interact solve

solve :: String -> String
solve input =
  let result1 = part1 input
      result2 = part2 input
  in unlines
       [ "Part 1: " ++ result1
       , "Part 2: " ++ result2
       ]

part1 :: String -> String
part1 input = "not implemented"

part2 :: String -> String
part2 input = "not implemented"
EOF
done

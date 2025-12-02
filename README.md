# Template for AoC in Hasekll
A very simple Haskell template for [Advent of Code](https://adventofcode.com/) 

It uses `make` and `ghc` instead of `cabal` or `stack` to keep things simple.

## At the start of the year
1. Clone this repo
2. Get your session cookie from the AoC website and put it in `.aoc_session`
3. Change the year in the `get_input.sh` script
4. Install useful dependencies
```
make run installdeps
```

## Each day
1. Run `./get_input.sh <day-number>` to download the input to `inputs/dayNN.txt`
2. Run your solution with `make run <day-number>` e.g. `make run 4`
3. Run your solution on sample input from `samples/dayNN.txt` with `make test <day-number>`
4. Add more dependencies with 
```
cabal update
cabal install --lib <package>
```




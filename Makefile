# Minimal Advent of Code Makefile (Haskell + stdin)
SRC      := src
BIN      := bin
INPUTS   := inputs
SAMPLES  := samples
GHC      := ghc

# Put .hi/.o into $(BIN) to avoid polluting src/
# -odir and -hidir are widely supported; if you prefer you can use -outputdir=$(BIN)
GHCFLAGS := -O2 -threaded -rtsopts -v0 -odir=$(BIN) -hidir=$(BIN) -isrc \
	-XNoImplicitPrelude -XTupleSections -XBangPatterns -XTypeApplications

.PHONY: run test clean distclean

$(BIN):
	mkdir -p $(BIN)

# Compile src/dayNN.hs -> bin/dayNN
$(BIN)/day%: $(SRC)/day%.hs | $(BIN)
	@echo "Compiling day $*..."
	@$(GHC) $(GHCFLAGS) -o $@ $< >/dev/null
	-rm -f $(BIN)/*.hi $(BIN)/*.o

all:
	@for b in $(BIN)/day*; do \
		[ -e "$$b" ] || continue; \
		[ -x "$$b" ] || { echo "Skipping $$(basename $$b): not executable"; continue; }; \
		day=$$(basename $$b); \
		num=$$(echo $$day | sed 's/day//'); \
		input=$(INPUTS)/$$day.txt; \
		if [ -f "$$input" ]; then \
			echo "====== Day $$num ======"; \
			cat "$$input" | ./$$b; \
		else \
			echo "Skipping Day $$num: no input $$input"; \
		fi; \
	done

# add more here
installdeps:
	cabal update
	cabal install --lib vector split text containers unordered-containers megaparsec hashable extra deepseq

# Run on real input: make run 10
run: AOC.hs
	@DAYNUM=$(word 2,$(MAKECMDGOALS)); \
	if [ -z "$$DAYNUM" ]; then echo "Usage: make run <day-number>"; exit 1; fi; \
	DAY=$$(printf "%02d" $$DAYNUM); \
	$(MAKE) $(BIN)/day$$DAY >/dev/null; \
	echo "====== Day $$DAY ======"; \
	cat $(INPUTS)/day$$DAY.txt | ./$(BIN)/day$$DAY
%::
	@:

# Run on sample input: make test 10
test: AOC.hs
	@DAYNUM=$(word 2,$(MAKECMDGOALS)); \
	if [ -z "$$DAYNUM" ]; then echo "Usage: make test <day-number>"; exit 1; fi; \
	DAY=$$(printf "%02d" $$DAYNUM); \
	$(MAKE) $(BIN)/day$$DAY >/dev/null; \
	echo "====== Day $$DAY (sample) ======"; \
	cat $(SAMPLES)/day$$DAY.txt | ./$(BIN)/day$$DAY
%::
	@:

# clean: remove binaries and any .hi/.o in bin and src
clean:
	rm -rf $(BIN) *.hi *.o
	-rm -f $(SRC)/*.hi $(SRC)/*.o

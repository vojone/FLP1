# Makefile for FLP1 project
# Author: Vojtech Dvorak (xdvora3o)

BIN_NAME = flp-fun

SOURCES = ArgumentParser.hs CommonParser.hs Parser.hs\
	MData.hs MDataParser.hs Classifier.hs DecisionTree.hs\
	DecisionTreeParser.hs Trainer.hs Main.hs

INZIP = Makefile README.md $(SOURCES)

ZIP_NAME = xdvora3o.zip

TEST_ZIP_NAME = public-tests.zip
TEST_DIR = public

COMPILER = ghc
FLAGS = --make -O1 -Wall

.PHONY:\
	run\
	prof\
	zip\
	test\
	clean

$(BIN_NAME): $(SOURCES)
	$(COMPILER) $(SOURCES) -o $@ $(FLAGS)

run:
	$(BIN_NAME)

# NOTE: Works only on the Merlin server
# Run with: ./$(BIN_NAME)-prof +RTS -p -RTS
prof:
	$(COMPILER) $(SOURCES) -prof -fprof-auto -rtsopts --make -Wall \
	-o $(BIN_NAME)-prof

zip:
	zip $(ZIP_NAME) $(INZIP)

$(TEST_DIR):
	unzip $(TEST_ZIP_NAME)

test: $(BIN_NAME) $(TEST_DIR)
	bash -c "cp $(BIN_NAME) public/$(BIN_NAME) && pushd $(TEST_DIR) && \
	python3.10 test_flp.py && popd"

clean:
	rm -f *.o *.hi $(BIN_NAME) $(BIN_NAME)-prof $(ZIP_NAME)

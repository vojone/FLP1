# Makefile for FLP1 project
# Author: Vojtech Dvorak (xdvora3o)

BIN_NAME = flp-fun

SOURCES = ArgumentParser.hs CommonParser.hs Parser.hs\
	MData.hs MDataParser.hs Classifier.hs DecisionTree.hs\
	DecisionTreeParser.hs Main.hs

INZIP = Makefile README.md $(SOURCES)

ZIP_NAME = xdvora3o.zip

TEST_ZIP_NAME = public-tests.zip

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
prof:
	$(COMPILER) $(SOURCES) -prof -fprof-auto -rtsopts --make -Wall \
	-o $(BIN_NAME)-prof

zip:
	zip $(ZIP_NAME) $(INZIP)

test: $(BIN_NAME)
	unzip $(TEST_ZIP_NAME) && cp $(BIN_NAME) public/$(BIN_NAME) &&\
	pushd public && python3.10 test_flp.py

clean:
	rm -f *.o *.hi $(BIN_NAME) $(ZIP_NAME)
	rm -r public

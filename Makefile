BIN_NAME = flp-fun

SOURCES = Trainer.hs ArgumentParser.hs BinaryDecisionTreeParser.hs Utils.hs\
	CSVParser.hs Classifier.hs Parser.hs DecisionTree.hs Main.hs

INZIP = Makefile README.md $(SOURCES)

ZIP_NAME = xdvora3o.zip

COMPILER = ghc
FLAGS = --make -O1 -Wall

.PHONY:\
	run\
	prof\
	zip\
	clean

$(BIN_NAME): $(SOURCES)
	$(COMPILER) $(SOURCES) -o $@ $(FLAGS)

run:
	$(BIN_NAME)

prof:
	$(COMPILER) $(SOURCES) -prof -fprof-auto -rtsopts $(FLAGS) -o $(BIN_NAME)-prof

zip:
	zip $(ZIP_NAME) $(INZIP)

clean:
	rm -f *.o *.hi $(BIN_NAME) $(ZIP_NAME)

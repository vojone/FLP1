BIN_NAME = flp-fun

SOURCES = Trainer.hs ArgumentParser.hs BinaryDecisionTreeParser.hs Utils.hs\
	DataParser.hs Classifier.hs Parser.hs DecisionTree.hs Main.hs

INZIP = Makefile README.md $(SOURCES)

ZIP_NAME = xdvora3o.zip


COMPILER = ghc
FLAGS = --make -Wall

.PHONY:\
	zip\
	clean\
	hi


$(BIN_NAME): $(SOURCES)
	$(COMPILER) $(SOURCES) -o $@ $(FLAGS)

run:
	$(BIN_NAME)

zip:
	zip $(ZIP_NAME) $(INZIP)

clean:
	rm -f *.o *.hi $(BIN_NAME) $(ZIP_NAME)

module_interfaces:
	ghc -fno-code -fwrite-interface $(FLAGS) $(SOURCES)

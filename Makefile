BIN_NAME = flp-fun
SOURCES = Trainer.hs ArgumentParser.hs TreeParser.hs Utils.hs DataParser.hs Classifier.hs Main.hs
INZIP = Makefile README.md $(SOURCES)
ZIP_NAME = xdvora3o.zip

OBJS = $(SOURCES:hs=o)

COMPILER = ghc
FLAGS = -Wall

.PHONY:\
	zip\
	clean\
	hi

%.o: %.hs module_interfaces
	$(COMPILER) $< -c $(FLAGS)

$(BIN_NAME): $(OBJS)
	$(COMPILER) $(OBJS) -o $@ $(FLAGS)

run:
	$(BIN_NAME)

zip:
	zip $(ZIP_NAME) $(INZIP)

clean:
	rm -f *.o *.hi $(BIN_NAME) $(ZIP_NAME)

module_interfaces:
	ghc -fno-code -fwrite-interface $(FLAGS) $(SOURCES)

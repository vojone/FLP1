BIN_NAME = flp-fun
SOURCES = ArgumentParser.hs main.hs
INZIP = Makefile README.md $(SOURCES)
ZIP_NAME = xdvora3o.zip

OBJS = $(SOURCES:hs=o)

COMPILER = ghc
FLAGS = -Wall

.PHONY:\
	zip\
	clean\
	hi

%.o: %.hs hi
	$(COMPILER) $< -c $(FLAGS)

$(BIN_NAME): $(OBJS)
	$(COMPILER) $(OBJS) -o $@ $(FLAGS)

run:
	$(BIN_NAME)

zip:
	zip $(ZIP_NAME) $(INZIP)

clean:
	rm -f *.o *.hi $(BIN_NAME) $(ZIP_NAME)

hi:
	ghc -fno-code -fwrite-interface $(SOURCES)

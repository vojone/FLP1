BIN_NAME = flp-fun
SOURCES = flp-fun.hs
INZIP = Makefile README.md $(SOURCES)
ZIP_NAME = xdvora3o.zip

OBJS = $(SOURCES:hs=o)

COMPILER = ghc
FLAGS = -Wall

.PHONY:\
	zip\
	clean

%.o: %.hs
	$(COMPILER) $< -c $@ $(FLAGS)

$(BIN_NAME): $(OBJS)
	$(COMPILER) $(OBJS) -o $@ $(FLAGS)

zip:
	zip $(ZIP_NAME) $(INZIP)

clean:
	rm -f *.o *.hi $(BIN_NAME) $(ZIP_NAME)

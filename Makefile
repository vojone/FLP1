BIN_NAME = flp-fun
SOURCES = flp-fun.hs
OBJS = $(SOURCES:hs=o)

COMPILER = ghc
FLAGS = -Wall

%.o: %.hs
	$(COMPILER) $< -c $@ $(FLAGS)

$(BIN_NAME): $(OBJS)
	$(COMPILER) $(OBJS) -o $@ $(FLAGS)

clean:
	rm -f *.o *.hi $(BIN_NAME)

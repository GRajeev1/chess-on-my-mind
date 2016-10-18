OBJECT_FILES = \
	book.o \
	search.o

all: tchess

tchess: $(OBJECT_FILES)
	g++ -O3 -o tchess $(OBJECT_FILES)

%.o: %.c chessdefs.h
	g++ -O3 -c $< -o $@



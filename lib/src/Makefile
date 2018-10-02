
SRC:=${wildcard *.adb}
OBJ:=${SRC:.adb=.o}

all: libfits_io.a

%.o: %.adb
	gnatmake $< -bargs static

libfits_io.a : $(OBJ)
	ar r libfits_io.a  $(OBJ)

clean:
	rm -f *.o *.ali libfits_io.a core

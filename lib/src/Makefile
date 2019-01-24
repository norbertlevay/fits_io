
SRC:=${wildcard fits-*.adb}
SRC+=fits.adb
OBJ:=${SRC:.adb=.o}

SRCIO:=${wildcard fits_io*.adb}
OBJIO:=${SRCIO:.adb=.o}

all: libfits_io.a libfitsio_io.a

%.o: %.adb
	gnatmake -gnat12  -g $< -bargs E static

libfits_io.a : $(OBJ)
	ar r libfits_io.a  $(OBJ)

libfitsio_io.a : $(OBJIO)
	ar r libfitsio_io.a  $(OBJIO)

clean:
	rm -f *.o *.ali libfits_io.a libfitsio_io.a core

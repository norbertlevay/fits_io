
INCDIR=../lib/src
LIBDIR=../lib
LIB=fits_io


all:
	gnatmake -g -gnat05 -we create.adb -I$(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat05 -we float64.adb -I$(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static


clean:
	rm -f float64.fit b~float64.ad? float64 create.fits create b~create.ad?  *.ali *.o *~

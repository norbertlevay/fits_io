
INCDIR=../lib/src
LIBDIR=../lib
LIB=fits_io


all:
	gnatmake -g -gnat05 -we create.adb -I$(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat05 -we convert.adb -I$(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static


clean:
	rm -f convert.fit b~convert.ad? convert create.fits create b~create.ad?  *.ali *.o *~


INCDIR=../lib/src
LIBDIR=../lib
LIB=fits_io

create:
	gnatmake -g -gnat05 -we create.adb -I$(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static


clean:
	rm -f create.fits create b~create.ad?  *.ali *.o *~

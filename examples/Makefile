
INCDIR=../lib/src
LIBDIR=../lib
LIB=fits_io

# removed: errored: function can have only in params
#-gnat05
#-gnat05
all:
	gnatmake -g -gnat12 -we create.adb -I$(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we convert.adb -I$(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static


clean:
	rm -f convert.fit b~convert.ad? convert create.fits create b~create.ad?  *.ali *.o *~

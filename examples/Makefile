
INCDIR=-I../lib/src -I../lib/src/parser -I../algorithm-dataaccess
LIBDIR=../lib/lib
LIB=fits_io

# removed: errored: function can have only in params
#-gnat05
#-gnat05
all:
	gnatmake -g -gnat12 -we create.adb  $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we convert.adb $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we convertf2i.adb $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we list.adb    $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we reserved_keys.adb    $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static


clean:
	rm -f convert.fit b~convert.ad? convertf2i convert create.fits convert.fits convertf2i.fits create b~create.ad? list b~list.ad? reserved_keys b~reserved_keys.ad?  *.ali *.o *~



run:
	./create
	fitsheader create.fits
	fits2bitmap create.fits
	gio open create.png
	./convert create.fits
	fitsheader convert.fits
	fits2bitmap convert.fits
	gio open convert.png
	./convertf2i convert.fits
	fitsheader convertf2i.fits
	fits2bitmap convertf2i.fits
	gio open convertf2i.png


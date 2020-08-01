
INCDIR=-I../lib/src -I../lib/src/parser -I../lib/src/data
LIBDIR=../lib/lib
LIB=fits_io

# removed: errored: function can have only in params
#-gnat05
#-gnat05
all:
	gnatmake -g -gnat12 -we create.adb  $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we convert.adb $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we list.adb    $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we reserved_keys.adb    $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we data_test.adb    $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static
	gnatmake -g -gnat12 -we data_print.adb    $(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -largs -lfits_io -bargs -E #-largs -static


clean:
	rm -f convert.fits convert create.fits create list reserved_keys b~*.ad?  *.ali *.o *~



run:
	./bin/create
	fitsheader bin/create.fits
	fits2bitmap bin/create.fits
	gio open bin/create.png
	./bin/convert bin/create.fits
	fitsheader bin/convert.fits
	fits2bitmap bin/convert.fits
	gio open bin/convert.png
	./bin/minmax bin/convert.fits


runcutout:
	./bin/cutoutalt_4 ../data/NANTEN2/FOCx38i0101t_c0f.fits 400 400 600 600 3.5
	./bin/cutoutalt_4 ../data/NANTEN2/WFPC2ASSNu5780205bx.fits 50 1 100 35 0.055555

testminmax:
	./bin/minmax_v3_alt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0001.fits
	./bin/minmax_v3_alt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0002.fits
	./bin/minmax_v3_alt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0003.fits
	./bin/minmax_v3_alt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0004.fits
	./bin/minmax_v3_alt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0005.fits
	./bin/minmax_v3_alt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0006.fits



testminmaxalt_4:
	./bin/minmaxalt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0001.fits
	./bin/minmaxalt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0002.fits
	./bin/minmaxalt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0003.fits
	./bin/minmaxalt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0004.fits
	./bin/minmaxalt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0005.fits
	./bin/minmaxalt_4 ../data/www.cv.nrao.edu/fits/data/tests/pg93/tst0006.fits





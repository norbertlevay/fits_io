


ZLIB_DIR=./png/libz-1.2.8.2015.12.26
ZLIBADA_DIR=./png/zlib-ada
PNG_DIR=./png/png_4_6
FITS_IO_DIR=./lib/src

INCDIR=../lib/src
LIBDIR=../lib
LIB=fits_io

all : fits

builddate=$(shell date)

build_date.ads :
	@echo "package Build_Date is" > build_date.ads
	@echo "BuildDate : constant String := \"${builddate}\";" >> build_date.ads
	@echo "end Build_Date;" >> build_date.ads

fits : build_date.ads
	gnatmake -g -gnat12 -we main.adb -o fits -I$(INCDIR) -aL$(LIBDIR) -aO$(LIBDIR) -I$(FITS_IO_DIR) -I$(ZLIBADA_DIR) -I$(PNG_DIR) -bargs -E -static -largs -lfits_io -L$(ZLIB_DIR) -lz

clean:
	rm -f build_date.ads fits b~main.ad?  *.ali *.o *~

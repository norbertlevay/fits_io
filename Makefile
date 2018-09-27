
builddate=$(shell date)

#TESTFILE=unimap_l118_blue_wglss_rcal.fits
TESTFILE=COHRS_11p00_0p00_CUBE_REBIN_R1.fit

# Dependecies:
# where /libz.a is, 
# and where ZLib-Ada and PNG sources and *.o are:
ZLIB_DIR=./png/libz-1.2.8.2015.12.26
ZLIBADA_DIR=./png/zlib-ada
PNG_DIR=./png/png_4_6

SRC=main.adb

TARGETS=fits examplecreatefitsfile testfits

all: $(TARGETS)
#testfits exampleCreateFitsFile


build_date.ads :
	@echo "package Build_Date is" > build_date.ads
	@echo "BuildDate : constant String := \"${builddate}\";" >> build_date.ads
	@echo "end Build_Date;" >> build_date.ads

fits : build_date.ads
	gnatmake -g -gnat05 -we $(SRC) -o fits -I$(ZLIBADA_DIR) -I$(PNG_DIR) -static -largs -L$(ZLIB_DIR) -lz -bargs -E

testfits:
	gnatmake -g -gnat05 -we testfits.adb -I$(ZLIBADA_DIR) -I$(PNG_DIR) -static -largs -L$(ZLIB_DIR) -lz -bargs -E

examplecreatefitsfile:
	gnatmake -g -gnat05 -we examplecreatefitsfile.adb -I$(ZLIBADA_DIR) -I$(PNG_DIR) -static -largs -L$(ZLIB_DIR) -lz -bargs -E

# before compiled with -gnat05 but to iterate over Data.Float32Arr in for cycles -gnat12 needed (see FITS to PNG)
# -bargs -E -> for addr2line --exe=./fits 0x...  at excpetion: -E is passed to binder (-bargs)
# -we turns warnings into errors
# -gnaty <-- prints warnings on identation style

ncubetest : ncubetest.adb ncube.ads ncube.adb
	gnatmake -g ncubetest.adb -o ncubetest

examplerun: exampleCreateFitsFile
	./exampleCreateFitsFile
	./fits png exampleCreateFitsFile.fits
	display exampleCreateFitsFile.fits_P1.png &


.PHONY: clean distclean

debug:
	gdb --batch --command=zlib.gdb --args ./fits png ngc6503.fits


test:
	rm *.png
	./fits png $(shell ls *.fits)
	./fits png --plane 2 WFPC2u5780205r_c0fx.fits
	./fits png --plane 3 WFPC2u5780205r_c0fx.fits
	./fits png --plane 4 WFPC2u5780205r_c0fx.fits
	./fits png --plane 15 ngc6503.fits

clean:
	rm -f  $(TARGETS) *.o *.ali build_date.*
#	rm -f fits testfits exampleCreateFitsFile *.o *.ali build_date.* b~*.ad? b~*.ad?

distclean: clean
	make distclean -C doc
	rm -f *~ test-modifyheader.hdr

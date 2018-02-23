
builddate=$(shell date)

#TESTFILE=unimap_l118_blue_wglss_rcal.fits
TESTFILE=COHRS_11p00_0p00_CUBE_REBIN_R1.fit

all: fits testfits exampleCreateFitsFile


build_date.ads :
	@echo "package Build_Date is" > build_date.ads
	@echo "BuildDate : constant String := \"${builddate}\";" >> build_date.ads
	@echo "end Build_Date;" >> build_date.ads

fits : main.adb build_date.ads options.ads options.adb commands.ads commands.adb fits.ads fits.adb fits-file.ads fits-file.adb fits-data.ads fits-data.adb fits-size.ads fits-size.adb
#	gnatmake -g -gnat12 main.adb -we -o fits -aI./png/zlib-ada -aI./png/png_4_6 -aO./png/zlib-ada -aO./png/png_4_6 -largs ./png/zlib-1.2.11/libz.a -bargs -E
#	gnatmake -g -gnat12 main.adb -we -o fits -aI./png/zlib-ada -aI./png/png_4_6 -aO./png/zlib-ada -aO./png/png_4_6 -largs -L /usr/lib/i386-linux-gnu -lz -bargs -E
	gnatmake -g -gnat12 main.adb -we -o fits -aI./png/zlib-ada -aI./png/png_4_6 -aO./png/zlib-ada -aO./png/png_4_6 -static -largs ./png/libz-1.2.8.2015.12.26/libz.a -bargs -E
# before compiled with -gnat05 but to iterate over Data.Float32Arr in for cycles -gnat12 needed (see FITS to PNG)
# -bargs -E -> for addr2line --exe=./fits 0x...  at excpetion: -E is passed to binder (-bargs)
# -we turns warnings into errors
# -gnaty <-- prints warnings on identation style

testfits : build_date.ads testfits.adb fits.ads fits.adb fits-file.ads fits-file.adb
	gnatmake -g -gnat12 testfits.adb -o testfits -aI./png/zlib-ada -aI./png/png_4_6 -aO./png/zlib-ada -aO./png/png_4_6 -largs -lz -bargs -E

exampleCreateFitsFile : build_date.ads examplecreatefitsfile.adb fits.ads fits.adb fits-file.ads fits-file.adb
	gnatmake -g -gnat12 examplecreatefitsfile.adb -o exampleCreateFitsFile -largs -bargs -E

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
	rm -f fits fitsio fitbio testfits testfits_dio testfits_io *.o *.ali build_date.* b~*.ad? b~*.ad?

distclean: clean
	make distclean -C doc
	rm -f *~ test-modifyheader.hdr

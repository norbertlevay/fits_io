

# note: git diff --exit-code : echo "$?" returns 1 if there were changes, and 0 if no changes

builddate=$(shell date)

#TESTFILE=unimap_l118_blue_wglss_rcal.fits
TESTFILE=COHRS_11p00_0p00_CUBE_REBIN_R1.fit

all: fits fitsio testfits_io testfits_sio testfits_dio


build_date.ads :
	@echo "package Build_Date is" > build_date.ads
	@echo "BuildDate : constant String := \"${builddate}\";" >> build_date.ads
	@echo "end Build_Date;" >> build_date.ads

fits : main.adb build_date.ads commands.ads commands.adb
	gnatmake -g -gnat05 -we main.adb -o fits

fitsio : main_io.adb build_date.ads commands_io.ads commands_io.adb
#	gnatmake -g -gnat12 -we main.adb -o fitsio -bargs -E
	gnatmake -g -gnat05 -we main_io.adb -o fitsio
# -we turns warnings into errors
# -gnaty <-- prints warnings on identation style

testfits_dio : build_date.ads testfits_dio.adb fits_dio.ads fits_dio.adb
	gnatmake -g -we testfits_dio.adb -o testfits_dio

testfits_sio : build_date.ads testfits_sio.adb fits_sio.ads fits_sio.adb
	gnatmake -g -we testfits_sio.adb -o testfits_sio

testfits_io : testfits_io.adb build_date.ads
#	gnatmake -g -gnat12 -we testfits_io.adb -o testfits_io -bargs -E
	gnatmake -g -gnat05 -we testfits_io.adb -o testfits_io

runtestfits_io : testfits_io main
	./testfits_io
	./fits info testfile.fits

testsameheader:   # orig header has 35 cards
	rm -f $(TESTFILE)
	cp $(TESTFILE).orig $(TESTFILE)
	chmod +w $(TESTFILE)
	./fits header $(TESTFILE) test-36cards.hdr

testbiggerheader:   # orig header has 35 cards
	rm -f $(TESTFILE)
	cp $(TESTFILE).orig $(TESTFILE)
	chmod +w $(TESTFILE)
	./fits header $(TESTFILE) test-50cards.hdr

testmodifyheader:
	./fits header --hdu 2 $(TESTFILE).orig | sed 's/SIMPLE/HUHUHU/'| sed 's/XTENSION/HUHUHUHU/'> test-modifyheader.hdr
	rm -f $(TESTFILE)
	cp $(TESTFILE).orig $(TESTFILE)
	chmod +w $(TESTFILE)
	./fits header --hdu 2 $(TESTFILE) test-modifyheader.hdr

clean:
	rm -f fitsio testfits_sio testfits_io *.o *.ali build_date.* b~*.ad? b~*.ad?


distclean: clean
	rm -f *~



# note: git diff --exit-code : echo "$?" returns 1 if there were changes, and 0 if no changes

builddate=$(shell date)

TARGET=fits

#TESTFILE=unimap_l118_blue_wglss_rcal.fits
TESTFILE=COHRS_11p00_0p00_CUBE_REBIN_R1.fit

all: main testfits_io fitsstreamtest


build_date.ads :
	@echo "package Build_Date is" > build_date.ads
	@echo "BuildDate : constant String := \"${builddate}\";" >> build_date.ads
	@echo "end Build_Date;" >> build_date.ads


main : main.adb build_date.ads
#	gnatmake -g -gnat12 -we main.adb -o ${TARGET} -bargs -E
	gnatmake -g -gnat05 -we main.adb -o ${TARGET} 
# -we turns warnings into errors
# -gnaty <-- prints warnings on identation style

fitsstreamtest : build_date.ads fitsstreamtest.adb fits.ads fits.adb
	gnatmake -g -we fitsstreamtest.adb -o fitsstreamtest

testfits_io : testfits_io.adb build_date.ads
#	gnatmake -g -gnat12 -we testfits_io.adb -o testfits_io -bargs -E
	gnatmake -g -gnat05 -we testfits_io.adb -o testfits_io

runtestfits_io : testfits_io main
	./testfits_io
	./fits info test.fits

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
	rm -f ${TARGET} fitsstreamtest testfits_io *.o *.ali build_date.* b~main.* b~testfits_io.*


distclean: clean
	rm -f *~

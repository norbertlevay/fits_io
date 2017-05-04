

builddate=$(shell date)

TARGET=fits

#TESTFILE=unimap_l118_blue_wglss_rcal.fits
TESTFILE=COHRS_11p00_0p00_CUBE_REBIN_R1.fit

all: main


build_date.ads :
	@echo "package Build_Date is" > build_date.ads
	@echo "BuildDate : constant String := \"${builddate}\";" >> build_date.ads
	@echo "end Build_Date;" >> build_date.ads


main : main.adb build_date.ads
	gnatmake -gnat12 -we  main.adb -o ${TARGET}
	rm build_date.ads
# -we turns warnings into errors
# -gnaty <-- prints warnings on identation style

testsameheader:   # orig header has 35 cards
	cp $(TESTFILE).orig $(TESTFILE)
	chmod +w $(TESTFILE)
	./fits header $(TESTFILE) test-35cards.hdr

testbiggerheader:   # orig header has 35 cards
	cp $(TESTFILE).orig $(TESTFILE)
	chmod +w $(TESTFILE)
	./fits header $(TESTFILE) test-50cards.hdr


clean:
	rm -f ${TARGET} *.o *.ali build_date.*


distclean:
	make clean
	rm -f *~


builddate=$(shell date)

#TESTFILE=unimap_l118_blue_wglss_rcal.fits
TESTFILE=COHRS_11p00_0p00_CUBE_REBIN_R1.fit

all: fits testfits


build_date.ads :
	@echo "package Build_Date is" > build_date.ads
	@echo "BuildDate : constant String := \"${builddate}\";" >> build_date.ads
	@echo "end Build_Date;" >> build_date.ads

fits : main.adb build_date.ads commands.ads commands.adb fits.ads fits.adb fits-file.ads fits-file.adb
	gnatmake -g -gnat05 -we main.adb -o fits -bargs -E
# -bargs -E ?? -> for addr2line ?? at excpetion
# -we turns warnings into errors
# -gnaty <-- prints warnings on identation style

testfits : build_date.ads testfits.adb fits.ads fits.adb fits-file.ads fits-file.adb
	gnatmake -g -we testfits.adb -o testfits


.PHONY: clean distclean


clean:
	rm -f fits fitsio testfits testfits_dio testfits_io *.o *.ali build_date.* b~*.ad? b~*.ad?

distclean: clean
	make distclean -C doc
	rm -f *~ test-modifyheader.hdr

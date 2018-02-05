
builddate=$(shell date)

#TESTFILE=unimap_l118_blue_wglss_rcal.fits
TESTFILE=COHRS_11p00_0p00_CUBE_REBIN_R1.fit

all: fits testfits


build_date.ads :
	@echo "package Build_Date is" > build_date.ads
	@echo "BuildDate : constant String := \"${builddate}\";" >> build_date.ads
	@echo "end Build_Date;" >> build_date.ads

fits : main.adb build_date.ads options.ads options.adb commands.ads commands.adb fits.ads fits.adb fits-file.ads fits-file.adb
	gnatmake -g -gnat12 -we main.adb -o fits -aI./png/zlib-ada -aI./png/png_4_6 -aO./png/zlib-ada -aO./png/png_4_6 -largs -lz -bargs -E -g
# before compiled with -gnat05 but to iterate over Data.Float32Arr in for cycles -gnat12 needed (see FITS to PNG)
# -bargs -E ?? -> for addr2line ?? at excpetion
# -we turns warnings into errors
# -gnaty <-- prints warnings on identation style

testfits : build_date.ads testfits.adb fits.ads fits.adb fits-file.ads fits-file.adb
	gnatmake -g -we testfits.adb -o testfits -aI./png/zlib-ada -aI./png/png_4_6 -aO./png/zlib-ada -aO./png/png_4_6 -largs -lz 


.PHONY: clean distclean


clean:
	rm -f fits fitsio testfits testfits_dio testfits_io *.o *.ali build_date.* b~*.ad? b~*.ad?

distclean: clean
	make distclean -C doc
	rm -f *~ test-modifyheader.hdr




-PHONY: all
all:
	gnatmake -P lib/fits_io.gpr -f
	gnatmake -P fits/fits.gpr -f
	make -C examples/ clean; make -C examples/



testpng:
	rm -f *.png
	./fits/fits png --plane 15 ./data/NANTEN2/ngc6503.fits
	./fits/fits png --plane 2 ./data/NANTEN2/WFPC2u5780205r_c0fx.fits
	./fits/fits png --plane 3 ./data/NANTEN2/WFPC2u5780205r_c0fx.fits
	./fits/fits png --plane 4 ./data/NANTEN2/WFPC2u5780205r_c0fx.fits


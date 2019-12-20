


-PHONY: all
all:
	gnatmake -P lib/fits_io.gpr -f
	gnatmake -P fits/fits.gpr -f
	make -C examples/ clean; make -C examples/

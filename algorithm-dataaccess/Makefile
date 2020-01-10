


all: data_test data_print

data_test:
	gnatmake data_test.adb -I../lib/src

data_print:
	gnatmake data_print.adb -I../lib/src -I../lib/src/parser


clean:
	rm -f *.ali *.o *~ data_test data_print



run:
	./data_print ../data/NANTEN2/ngc6503.fits

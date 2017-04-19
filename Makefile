

builddate=$(shell date)

TARGET=fits

all: main


build_date.ads :
	@echo "package Build_Date is" > build_date.ads
	@echo "BuildDate : constant String := \"${builddate}\";" >> build_date.ads
	@echo "end Build_Date;" >> build_date.ads


main : main.adb build_date.ads
	gnatmake -we  main.adb -o ${TARGET}
	rm build_date.ads
# -we turns warnings into errors
# -gnaty <-- prints warnings on identation style

clean:
	rm -f ${TARGET} *.o *.ali build_date.*


distclean:
	make clean
	rm -f *~

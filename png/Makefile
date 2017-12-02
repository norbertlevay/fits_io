

libz-1.2.8.2015.12.26.tar.gz:
	wget https://sortix.org/libz/release/libz-1.2.8.2015.12.26.tar.gz

libz-1.2.8.2015.12.26: libz-1.2.8.2015.12.26.tar.gz
	tar xfzv libz-1.2.8.2015.12.26.tar.gz

# FIXME
buildlibz: libz-1.2.8.2015.12.26
	cd libz-1.2.8.2015.12.26
	./configure 
	make
	cd -


zlib-ada:
	mkdir zlib-ada
	tar xzfv zlib-ada-1.3.tar.gz -C zlib-ada
	gnatmake -Pzlib-ada/zlib.gpr


png_4_6:
	unzip png_4_6.zip
	cp Microscope.png png_4_6/
	cp Makefile.png_4_6 png_4_6/Makefile
	make -C png_4_6




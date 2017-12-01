
git clone https://aur.archlinux.org/libz.git 

# This is an ArchLinux/AUR pack:
# makepkg -si stops with conflict error: libz conflicts zlib
# ok -> so I did not install, kept the compiled binaries and 
# linked zlib-ada (and so png_4_6) against them.


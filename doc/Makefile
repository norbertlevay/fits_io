
# Install Latex build env:
# 1, install full TexLive from Mint/Ubuntu
# 2, IA2 logo and ia2-template.cls go to ~/texmf/tex/latex/ia2-template/
# 3, needs minted env and that needs pygments:
#    apt-get install python-pygments
# 4, pdflatex needs --shell-escape

# build 3 times to resolve cross references

TARGETS=ada-extfile-access.pdf

#all: $(TARGETS)
#	pdflatex --shell-escape ada-extfile-access.tex


ada-extfile-access.pdf : ada-extfile-access.tex
	pdflatex --shell-escape ada-extfile-access.tex
	pdflatex --shell-escape ada-extfile-access.tex
	pdflatex --shell-escape ada-extfile-access.tex

# note: dia figures must be exported to eps and then converted to pdf 
#       this avoids empty space around figure
#vlkb_radiocube_services.pdf:
#	dia -e vlkb_radiocube_services.eps -t eps vlkb_radiocube_services.dia 
#	epspdf vlkb_radiocube_services.eps


pdf2text: 
	pdftotext -layout vialactea_wp5_OATs_vlkb_cubes_service_v0.3.pdf


clean:
	rm -f *.out *.log *.aux *.toc *.pyg

distclean: clean
	rm -f $(TARGETS) *~


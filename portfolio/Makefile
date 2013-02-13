ROOT = ..

MAKEINDEX = makeindex -q
TEX = pdflatex -shell-escape -interaction=nonstopmode -file-line-error

RERUN = "(There were undefined references|Rerun to get (cross-references|the bars) right)"

SRC=portfolio.tex
PDF=$(SRC:%.tex=%.pdf)
LOG=$(SRC:%.tex=%.log)
TEXFILES = $(shell find . -name '*.tex')

.PHONY : pdf download appendix

pdf : $(TEXFILES) 
	make clean
	mkdir -p build
	bin/porttree.rb .. build/portfolio_dir.tex
	$(TEX) $(SRC) 
	$(MAKEINDEX) courseterm.idx
	$(MAKEINDEX) coursecat.idx

	while ($(TEX) $(SRC) ; \
		grep -q $(RERUN) $(LOG) ) do true; \
		done

	$(TEX) $(SRC) # once more for luck...
	cp $(PDF) $(ROOT)

all : pdf

clean :
	rm -f *.idx *.log *.ilg *.aux *.out *.toc *.pdf *.ind
	$(shell find . -name '*.aux' -exec rm \{} \;)
	rm -rf build


## UTIL
help:
	@echo "help\t\tshow this message"
	@echo "pdf\t\tcreate pdf"
	@echo "download\t\tdownload all video files"
	@echo "clean\t\tclean this directory"



## VIDEOS
CHECKSUM = $(ROOT)/.checksum
VIDS_URL = "http://stout.hampshire.edu/~acg10/"

download : $(CHECKSUM)
	bin/download_vids.sh $(CHECKSUM) $(VIDS_URL)

%.pdf: %.tex
	pdflatex $<
	pdfcrop $@ crop-$@
	mv crop-$@ $@

%.svg: %.pdf
	pdf2svg $< $@

all: sail.svg

clean:
	rm -f *.aux *.log *.eps *.dvi *.pdf *.nav *.out *.snm


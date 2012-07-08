%.pdf: %.tex
	pdflatex $<
	pdfcrop $@ crop-$@
	mv crop-$@ $@

%.svg: %.pdf
	pdf2svg $< $@

all: random-problems-arrows.svg random-problems-legend.svg  \
	random-problems-arrows+legend.svg benchmarks.svg

clean:
	rm -f *.aux *.log *.eps *.dvi *.pdf *.nav *.out *.snm


all: paper.pdf

%.svg: %.pdf
	pdfcrop $< $<-cropped
	mv $<-cropped $<
	pdf2svg $< $@

paper.bbl: paper.tex refs.bib paper.aux
	bibtex paper

paper.aux: paper.tex
	pdflatex paper

paper.pdf: paper.tex paper.bbl flat.pdf flat.svg vct-wins.pdf vct-wins.svg 
	pdflatex paper
	pdflatex paper


clean:
	rm -f paper.pdf *.bbl *.log *.aux *.blg

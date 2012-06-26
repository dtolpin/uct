all: paper.pdf

%.pdf: %.tex
	pdflatex $<

%.svg: %.pdf
	pdfcrop $< $<-cropped
	mv $<-cropped $<
	pdf2svg $< $@

paper.bbl: paper.tex refs.bib paper.aux
	bibtex paper

paper.aux: paper.tex voi-wins.pdf uctvoi.pdf
	pdflatex paper

paper.pdf: paper.tex paper.bbl flat.pdf flat.svg voi-wins.pdf voi-wins.svg uctvoi.pdf uctvoi.svg
	pdflatex paper
	pdflatex paper


clean:
	rm -f paper.pdf *.bbl *.log *.aux *.blg

all: paper.pdf onbetterbound.pdf

%.pdf: %.tex
	pdflatex $<

%.svg: %.pdf
	pdfcrop $< $<-cropped
	mv $<-cropped $<
	pdf2svg $< $@

paper.pdf: paper.tex refs.bib figures
	pdflatex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

figures: 

clean:
	rm *.bbl paper.pdf *.log *.aux

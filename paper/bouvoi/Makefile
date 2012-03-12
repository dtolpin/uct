all: paper.pdf

paper.bbl: paper.tex refs.bib paper.aux
	bibtex paper

paper.aux: paper.tex
	pdflatex paper

paper.pdf: paper.tex
	pdflatex paper
	pdflatex paper


clean:
	rm -f paper.pdf *.bbl *.log *.aux *.blg

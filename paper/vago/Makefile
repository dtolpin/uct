all: paper.pdf

paper.bbl: paper.tex refs.bib paper.aux
	bibtex paper

paper.aux: paper.tex
	pdflatex paper

paper.pdf: paper.tex paper.bbl bests-bw.pdf bests-colorful.pdf \
			ect-wins.pdf vct-wins.pdf bct-wins.pdf 
	pdflatex paper
	pdflatex paper


clean:
	rm -f paper.pdf *.bbl *.log *.aux *.blg

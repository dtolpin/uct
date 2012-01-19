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
<<<<<<< HEAD
	rm *.bbl paper.pdf *.log *.aux
=======
	rm -f paper.pdf *.bbl *.log *.aux *.blg
>>>>>>> 74e68d408bfd7f974b46409a66d7a6d5d9362b72

all: slides.pdf

slides.pdf: slides.tex
	pdflatex $<
	pdflatex $<

clean:
	rm -f *.snm *.out *.log *.aux *.nav *.toc

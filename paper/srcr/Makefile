all: paper.pdf

%.svg: %.pdf
	pdf2svg $< $@

%.pdf: %.tex
	pdflatex $<

.PHONY: clean

paper.pdf: paper.tex refs.bib figures
	pdflatex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

figures: costs-size\=6-group\=median.pdf \
	costs-size\=6-group\=minimum.pdf \
	costs-size\=10-nsamples\=397.pdf \
	costs-size\=6-nsamples\=1585.pdf \
	costs-size\=6-nsamples\=397.pdf \
	flat-trilevel-k\=64-uqb\=8.pdf \
	onelevel-tree.pdf \
	tree-identity-k\=16-uqb\=8.pdf \
	tree-identity-k\=64-uqb\=8.pdf \
	twolevel-tree.pdf \
	tree-identity-k\=32-uqb\=8+voi.pdf

clean:
	rm paper.pdf *.log *.aux *.blg

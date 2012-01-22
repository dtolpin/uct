all: paper.pdf

%.pdf: %.tex
	pdflatex $<

%.svg: %.pdf
	pdfcrop $< $<-cropped
	mv $<-cropped $<
	pdf2svg $< $@

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
	rcq-size\=6-nsamples\=1585.pdf \
	rcq-size\=6-nsamples\=397.pdf \
	tree-identity-k\=16-uqb\=8.pdf \
	tree-identity-k\=64-uqb\=8.pdf \
	twolevel-tree.pdf \
	tree-identity-k\=32-uqb\=8+voi.pdf \
	costs-size\=6-group\=median.svg \
	costs-size\=6-group\=minimum.svg \
	costs-size\=10-nsamples\=397.svg \
	costs-size\=6-nsamples\=1585.svg \
	costs-size\=6-nsamples\=397.svg \
	flat-trilevel-k\=64-uqb\=8.svg \
	onelevel-tree.svg \
	rcq-size\=6-nsamples\=1585.svg \
	rcq-size\=6-nsamples\=397.svg \
	tree-identity-k\=16-uqb\=8.svg \
	tree-identity-k\=64-uqb\=8.svg \
	twolevel-tree.svg \
	tree-identity-k\=32-uqb\=8+voi.svg

clean:
	rm *.bbl paper.pdf *.log *.aux

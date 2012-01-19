all: paper.pdf

%.pdf: %.tex
	pdflatex $<

%.svg: %.pdf
	pdfcrop $< $<-cropped
	mv $<-cropped $<
	pdf2svg $< $@

.PHONY: figures clean

paper.pdf: paper.tex refs.bib figures
	pdflatex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

figures: \
	figurescosts-size=10-nsamples=397.pdf \
	costs-size=3-nsamples=397.pdf \
	costs-size=6-group=median.pdf \
	costs-size=6-group=minimum.pdf \
	costs-size=6-nsamples=1585.pdf \
	costs-size=6-nsamples=199.pdf \
	costs-size=6-nsamples=397.pdf \
	costs-size=6-nsamples=793.pdf \
	flat-trilevel-k=64-uqb=8.pdf \
	flat-trilevel-vct-k=64.pdf \
	onelevel-tree.pdf \
	rcq-size=6-nsamples=1585.pdf \
	rcq-size=6-nsamples=199.pdf \
	rcq-size=6-nsamples=397.pdf \
	rcq-size=6-nsamples=793.pdf \
	tree-identity-k=16-uqb=8.pdf \
	tree-identity-k=64-uqb=8.pdf \
	twolevel-tree.pdf

clean:
	rm *.bbl paper.pdf *.log *.aux

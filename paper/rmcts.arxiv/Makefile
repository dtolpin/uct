all: paper.pdf

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

figures: \
	baskets-n+1.pdf baskets-n+1.svg \
	baskets-p-m-g.pdf baskets-p-m-g.svg \
	costs-size=10-group=median.pdf costs-size=10-group=median.svg \
	costs-size=10-group=minimum.pdf costs-size=10-group=minimum.svg \
	costs-size=10-nsamples=397.pdf costs-size=10-nsamples=397.svg \
	costs-size=3-group=median.pdf costs-size=3-group=median.svg \
	costs-size=3-group=minimum.pdf costs-size=3-group=minimum.svg \
	costs-size=3-nsamples=397.pdf costs-size=3-nsamples=397.svg \
	costs-size=6-group=median.pdf costs-size=6-group=median.svg \
	costs-size=6-group=minimum.pdf costs-size=6-group=minimum.svg \
	costs-size=6-nsamples=1585.pdf costs-size=6-nsamples=1585.svg \
	costs-size=6-nsamples=199.pdf costs-size=6-nsamples=199.svg \
	costs-size=6-nsamples=397.pdf costs-size=6-nsamples=397.svg \
	costs-size=6-nsamples=793.pdf costs-size=6-nsamples=793.svg \
	flat-low-key-k=16-uqb=8.pdf flat-low-key-k=16-uqb=8.svg \
	flat-low-key-k=32-uqb=8.pdf flat-low-key-k=32-uqb=8.svg \
	flat-low-key-k=64-uqb=8.pdf flat-low-key-k=64-uqb=8.svg \
	flat-trilevel-k=16-uqb=8.pdf flat-trilevel-k=16-uqb=8.svg \
	flat-trilevel-k=32-uqb=8.pdf flat-trilevel-k=32-uqb=8.svg \
	flat-trilevel-k=64-uqb=8.pdf flat-trilevel-k=64-uqb=8.svg \
	flat-trilevel-vct-k=64.pdf flat-trilevel-vct-k=64.svg \
	onelevel-tree.pdf onelevel-tree.svg \
	rcq-size=6-nsamples=1585.pdf rcq-size=6-nsamples=1585.svg \
	rcq-size=6-nsamples=199.pdf rcq-size=6-nsamples=199.svg \
	rcq-size=6-nsamples=397.pdf rcq-size=6-nsamples=397.svg \
	rcq-size=6-nsamples=793.pdf rcq-size=6-nsamples=793.svg \
	tree-identity-k=16-uqb=8.pdf tree-identity-k=16-uqb=8.svg \
	tree-identity-k=32-uqb=8.pdf tree-identity-k=32-uqb=8.svg \
	tree-identity-k=64-uqb=8.pdf tree-identity-k=64-uqb=8.svg \
	tree-trilevel-k=16-uqb=8.pdf tree-trilevel-k=16-uqb=8.svg \
	tree-trilevel-k=32-uqb=8.pdf tree-trilevel-k=32-uqb=8.svg \
	tree-trilevel-k=64-uqb=8.pdf tree-trilevel-k=64-uqb=8.svg \
	twolevel-tree.pdf twolevel-tree.svg

clean:
	rm *.bbl paper.pdf *.log *.aux

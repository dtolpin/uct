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

figures: baskets-n+1.pdf baskets-n+1.svg \
	baskets-p-m-g.pdf baskets-p-m-g.svg \
	onelevel-tree.pdf onelevel-tree.svg \
	twolevel-tree.pdf twolevel-tree.svg \
	onelevel-64.pdf onelevel-64.svg \
	twolevel-64.pdf twolevel-64.svg \
	onelevel-8-16-32-64.pdf onelevel-8-16-32-64.svg \
	twolevel-8-16-32-64.pdf twolevel-8-16-32-64.svg \
	vararm-twolevel-17-49.pdf vararm-twolevel-17-49.svg \
	sailing-size=13-factor=_5625.pdf sailing-size=13-factor=_5625.svg \
	sailing-size=13-nsamples=240.pdf sailing-size=13-nsamples=240.svg \
	sailing-size=8-factor=_8438.pdf sailing-size=8-factor=_8438.svg \
	sailing-size=8-nsamples=185.pdf sailing-size=8-nsamples=185.svg

clean:
	rm *.bbl paper.pdf *.log *.aux
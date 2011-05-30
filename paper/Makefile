%.pdf: %.tex
	pdflatex $<

%.svg: %.pdf
	pdfcrop $< $<-cropped
	mv $<-cropped $<
	pdf2svg $< $@

all: figures

figures: baskets-n+1.pdf baskets-n+1.svg baskets-p-m-g.pdf baskets-p-m-g.svg twolevel-tree.pdf twolevel-tree.svg vararm-twolevel-17-49.svg twolevel-8-16-32-64.svg

%.pdf: %.tex
	pdflatex $<
	pdfcrop $@ crop-$@
	mv crop-$@ $@

%.svg: %.pdf
	pdf2svg $< $@-width-height
	sed '/<svg/ s/width="[^\"]*"\|height="[^\"]*"//g' < $@-width-height > $@
	rm $@-width-height

all: mcts-tree.svg go-board.svg voi-wins.svg flat.svg uctvoi.svg

clean:
	rm -f *.aux *.log *.eps *.dvi *.pdf *.nav *.out *.snm


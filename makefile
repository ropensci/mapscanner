LFILE = README
PAPER=paper

all: knith #open 

knith: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).html')" | R --no-save -q

knitr: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',rmarkdown::md_document(variant='gfm'))" | R --no-save -q

paper: $(PAPER).md
	pandoc --filter pandoc-citeproc -s $(PAPER).md -o $(PAPER).html

open: $(LFILE).html
	xdg-open $(LFILE).html &

clean:
	rm -rf *.html *.png README_cache 

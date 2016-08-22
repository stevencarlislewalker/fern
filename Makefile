all: README.md README.html clean

README.md: README.Rmd
	Rscript -e "library(knitr); knit(\"README.Rmd\")"

README.html: README.md
	Rscript -e "library(rmarkdown); render(\"README.Rmd\")"

clean:
	rm -f *.aux *.log *.pdf *.tex

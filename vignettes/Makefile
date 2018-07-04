all:
	$(R_HOME)/bin/Rscript -e "knitr::knit2pdf('dimensionality-reduction.Rnw')"
	$(R_HOME)/bin/Rscript -e "tools::compactPDF('dimensionality-reduction.pdf', gs_quality = 'ebook')"
	rm -rf dimensionality-reduction.tex figure/ auto/

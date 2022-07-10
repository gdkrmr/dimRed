all:
	echo "BNET_BUILD_VIGNETTE: $(BNET_BUILD_VIGNETTE)"
	echo "_R_CHECK_DEPENDS_ONLY_: " $(_R_CHECK_DEPENDS_ONLY_)
	[ _R_CHECK_DEPENDS_ONLY_ = "true" ] && $(R_HOME)/bin/Rscript -e "knitr::knit2pdf('dimensionality-reduction.Rnw')"
	[ _R_CHECK_DEPENDS_ONLY_ = "true" ] && $(R_HOME)/bin/Rscript -e "tools::compactPDF('dimensionality-reduction.pdf', gs_quality = 'ebook')"
	[ _R_CHECK_DEPENDS_ONLY_ = "true" ] && rm -rf dimensionality-reduction.tex figure/ auto/

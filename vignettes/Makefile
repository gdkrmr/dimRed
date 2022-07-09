
ifndef _R_CHECK_DEPENDS_ONLY_
	_R_CHECK_DEPENDS_ONLY_ = "false"
endif

all:
ifneq ($(_R_CHECK_DEPENDS_ONLY_), "true")
	echo "BNET_BUILD_VIGNETTE: $(BNET_BUILD_VIGNETTE)"
	$(R_HOME)/bin/Rscript -e "knitr::knit2pdf('dimensionality-reduction.Rnw')"
	$(R_HOME)/bin/Rscript -e "tools::compactPDF('dimensionality-reduction.pdf', gs_quality = 'ebook')"
	rm -rf dimensionality-reduction.tex figure/ auto/
endif

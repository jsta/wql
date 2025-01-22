.PHONY: docs check submit

README.md: README.Rmd
	Rscript -e 'rmarkdown::render($<, output_dir = ".", clean = TRUE)'

docs:
	Rscript -e 'pkgdown::build_site(".")'

check:
	Rscript -e 'rhub::rhub_check(platforms=1)'

submit:
	Rscript -e 'devtools::release()'
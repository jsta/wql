.PHONY: docs

README.md: README.Rmd
	Rscript -e 'rmarkdown::render($<, output_dir = ".", clean = TRUE)'

docs:
	Rscript -e 'pkgdown::build_site(".")'

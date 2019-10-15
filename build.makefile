quick: browserCode docs install

all:  docs vig build install check biocCheck

browserCode:
	(cd inst/browserCode; make)

docs:
	R -e "devtools::document()"
vig:
	R -e "devtools::build_vignettes()"

build:
	(cd ..; R CMD build --no-build-vignettes RCyjs)

install:
	(cd ..; R CMD INSTALL RCyjs)

check:
	(cd ..; R CMD check --no-manual --no-build-vignettes --ignore-vignettes `ls -t RCyjs_* | head -1`)

biocCheck:
	(cd ..; R CMD BiocCheck `ls -t RCyjs_* | head -1`)

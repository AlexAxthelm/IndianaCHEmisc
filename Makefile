all: test

document:
	R -e "devtools::document()"

build: document
	R -e "devtools::build()"

test: build
	R -e "devtools::test()"

TEX_FILES = $(shell find . -type f -name '*.tex')
BIB_FILES = $(shell find . -type f -name '*.bib')

all: main.pdf

main.pdf: $(TEX_FILES) $(BIB_FILES)
	@arara -l main

watch:
	@entr -s arara -l main

.PHONY: all watch

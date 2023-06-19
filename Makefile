TEX_FILES = $(shell find . -type f -name '*.tex')
BIB_FILES = $(shell find . -type f -name '*.bib')
ASSETS = $(shell find assets)

all: main.pdf

main.pdf: $(TEX_FILES) $(BIB_FILES) $(ASSETS)
	@arara -l main

debug:
	@arara -l -v main

watch:
	@entr -s arara -l main

.PHONY: all watch debug

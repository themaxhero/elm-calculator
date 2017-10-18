ELM  = ./node_modules/.bin/elm-make
FMT  = ./node_modules/.bin/elm-format

.PHONY: default dev build

default: dev

dev:
	$(FMT) --yes ./src
	$(ELM) --debug --warn --output index.html src/Main.elm

build:
	$(FMT) --validate ./src
	$(ELM) --warn --output index.html src/Main.elm
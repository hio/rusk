RUSK_BIN ?= ./target/debug/rusk
RUSK ?= $(RUSK_BIN)
PANDOC ?= pandoc

modules = example hello

all: build

build: build-debug readme html
test:
	cargo $@ $(ARGS)

build-all: build-debug build-release

build-debug:
	cargo build

build-release:
	cargo build --release

run: | _build
	cargo $@ $(ARGS)

_build:
	mkdir $@

.PHONY: $(modules)

html: $(modules)

readme: _build/README.html
_build/README.html: README.md | _build
	$(PANDOC) --template share/template.html --css ./style.css --metadata pagetitle:'Rusk: a KML dialect' $< -o $@

serve:
	cd _build && python3 -m http.server

$(modules): %: _build/%.rsk _build/%.log _build/%.md _build/%.html

_build/%.rsk: %.rsk | _build
	cp -a $< $@

_build/%.log: _build/%.rsk $(RUSK_BIN)
	$(RUSK) --debug $< 2>&1 | tee $@

_build/%.md: _build/%.log
	sed -e '1,/markdown/ d' < $< > $@

_build/%.html: _build/%.md _build/style.css
	$(PANDOC) --template share/template.html --css ./style.css $< -o $@

_build/style.css: share/style.css
	cp -a $< $@

RUSK_BIN ?= ./target/debug/rusk
RUSK ?= $(RUSK_BIN)
PANDOC ?= pandoc
CARGO ?= cargo

SEND_HOST?=send-host.localdomain

HTTP_SERVE ?= sh -x -c 'cd $$1 && $(PYTHON3) -m http.server $$2' '(http-serve)'
PYTHON3 ?= python3
PORT ?= 8000

all: build html

build clean run test update:
	$(CARGO) $@ $(ARGS)

release-bin:
	$(CARGO) build --release
	$(CARGO) build --release --target x86_64-unknown-linux-musl

send-all: send-src send-bin

send-src:
	rsync --exclude target --exclude _build --exclude .git --exclude del --exclude '*.swp' -avz . $(SEND_HOST):rusk/

send-bin:
	cp target/x86_64-unknown-linux-musl/release/rusk target/x86_64-unknown-linux-musl/release/rusk.static
	ls -l --si target/release/rusk target/x86_64-unknown-linux-musl/release/rusk.static
	rsync -v target/release/rusk target/x86_64-unknown-linux-musl/release/rusk.static $(SEND_HOST):rusk/

.PHONY: doc
doc: cargo-doc doc-html
cargo-doc:
	$(CARGO) doc $(ARGS)

modules ?= example hello t0 test

_build:
	mkdir $@

_build/doc: | _build
	mkdir $@

.PHONY: $(modules:%=html-%)

html: $(modules:%=html-%) doc-html

DOC_FILES = \
	README.md \
	doc/lib.md \
	doc/syntax.md \
	$(NULL)

MORE_DOC_OUTS = \
	ChangeLog.html \
	MEMO.html  \
	TODO.html \
	$(NULL)


doc-html: $(DOC_FILES:%.md=_build/%.html) $(MORE_DOC_OUTS:%=_build/%)

_build/ChangeLog.md: ChangeLog | _build
	cp $< $@

_build/MEMO.md: MEMO.txt | _build
	cp $< $@

_build/TODO.md: TODO.md | _build
	cp $< $@

metadata.syntax = --metadata pagetitle:'Rusk Syntax'

$(DOC_FILES:%.md=_build/%.html): _build/%.html: _build/%.md
	$(PANDOC) --toc --standalone --css `dirname $(@D) | sed -e 's,[_a-z][^/]*,..,'`/style.css --metadata source-file:$(<F) $(metadata.$(<:doc/%.md=%)) $< -o $@

$(DOC_FILES:%.md=_build/%.md): _build/%.md: %.md | _build
	cp $< $@.tmp
#	perl -i -ple 's,(\]\(\./.*)\.md\),$$1.html),' $@.tmp
	sed -i -e 's/\.md)/.html)/' $@.tmp
	sed -i -e 's/(ChangeLog)/(ChangeLog.html)/' $@.tmp
	mv $@.tmp $@

serve:
	$(HTTP_SERVE) _build $(PORT)

$(modules:%=html-%): html-%: _build/%.rsk _build/%.log _build/%.md _build/%.html

_build/%.rsk: example/%.rsk | _build/doc
	cp -a $< $@

_build/%.log: _build/%.rsk $(RUSK_BIN)
	$(RUSK) --debug $< 2>&1 | tee $@

_build/%.md: _build/%.log
	sed -e '1,/markdown/ d' < $< > $@

_build/%.html: _build/%.md _build/style.css
	$(PANDOC) --toc --standalone --css ./style.css $< -o $@

_build/style.css: example/style.css
	cp -a $< $@

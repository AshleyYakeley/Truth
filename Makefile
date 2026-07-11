default: full

PINAFOREVERSION := 0.6

# must be three numbers, add .0 as necessary
PINAFOREVERSIONABC := $(PINAFOREVERSION).0

SNAPSHOT := lts-24.45

### Flags for stack

ifeq ($(nodocker),1)
DOCKERFLAGS := --no-docker
else
DOCKERFLAGS :=
endif

ifeq ($(single),1)
JOBFLAGS := --no-keep-going --jobs 1
else
JOBFLAGS := --keep-going
endif

STACKROOTFLAGS := --stack-root $$PWD/.stack-root

STACK := stack $(STACKROOTFLAGS) $(DOCKERFLAGS) $(JOBFLAGS) --ta --hide-successes

ifeq ($(test),1)
TESTFLAGS :=
else
TESTFLAGS := --no-run-tests
endif

ifeq ($(bench),1)
BENCHFLAGS := --ba "--json benchmarks.json"
else
BENCHFLAGS := --no-run-benchmarks
endif

ifeq ($(haddock),1)
HADDOCKFLAGS := --haddock
else
HADDOCKFLAGS :=
endif

BINPATH := .build/bin

NIXFLAGS ?=

### Docker image

.PHONY: docker-image
docker-image:
ifeq ($(nodocker),1)
else
	docker build --build-arg SNAPSHOT=$(SNAPSHOT) -t local-build_$(SNAPSHOT) docker
endif

docker-shell: docker-image
	$(STACK) exec -- bash

### Formatting

${BINPATH}/fourmolu: docker-image
	$(STACK) install fourmolu

.PHONY: fourmolu
fourmolu: ${BINPATH}/fourmolu

.PHONY: format
format: ${BINPATH}/fourmolu
	env BINPATH=${BINPATH} $(STACK) --docker-env BINPATH exec -- bin/format-all


### Python

.build/python: docker-image
	$(STACK) exec -- python3 -m venv $@
	$(STACK) exec -- $@/bin/pip install -U setuptools wheel build sphinx myst-parser sphinx-rtd-theme pygments scour


### Licensing info

out:
	mkdir -p $@

out/licensing: out stack.yaml
	stack ls dependencies text --license | awk '{t=$$1;$$1=$$2;$$2=t;print}' | sort > $@

out/dependencies: out stack.yaml
	stack ls dependencies json > $@

.PHONY: dep-info
dep-info: out/licensing out/dependencies


### Haddock

.PHONY: haddock
haddock:
	mkdir -p stackpath
	ln -sf $(shell $(STACK) path --snapshot-doc-root) stackpath/snapshot-doc-root
	ln -sf $(shell $(STACK) path --local-doc-root) stackpath/local-doc-root
	$(STACK) haddock


### Watch-Building for development

stack-hls.yaml: stack.yaml
	yq '.docker = {"enable": false} | .["system-ghc"] = true | .["install-ghc"] = false' $< > $@

hie.yaml: stack-hls.yaml
	printf 'cradle:\n' > $@
	printf '  stack:\n' >> $@
	printf '    stackYaml: %s\n' "$<" >> $@
	printf '    components:\n' >> $@
	STACK_YAML="$<" gen-hie --stack | sed '1,2d;s/^    /      /' >> $@

.PHONY: hie-yaml
hie-yaml: hie.yaml

.PHONY: watch-build
watch-build: out docker-image
	$(STACK) build --file-watch --fast


### Executables

${BINPATH}/pinafore1 ${BINPATH}/pinadata ${BINPATH}/pinadoc &: out docker-image
ifeq ($(nodocker),1)
else
	rm -rf out/logs
ifeq ($(haddock),1)
	rm -rf out/haddock
endif
endif
	xhost +si:localuser:$${USER}
	$(STACK) install --ghc-options="-j" --test --bench $(TESTFLAGS) $(BENCHFLAGS) $(HADDOCKFLAGS)
	strip --remove-section=.comment ${BINPATH}/pinafore1
	strip --remove-section=.comment ${BINPATH}/pinadoc
ifeq ($(nodocker),1)
else
ifeq ($(haddock),1)
	cp -r `$(STACK) path --local-doc-root` out/haddock
endif
endif
ifeq ($(bench),1)
	test -n "$$(git status -s)" || ($(STACK) exec -- benchgraph/adapters/criterion/export_benchs.sh Pinafore/pinafore-app/benchmarks.json > benchmarks/pinafore-`git rev-parse HEAD`.ndjson)
endif

.PHONY: exe
exe: ${BINPATH}/pinafore1


### Debian package

PACKAGENAME := pinafore
PACKAGEVERSION := $(PINAFOREVERSION)
PACKAGEREVISION := 1
PACKAGEFULLNAME := $(PACKAGENAME)_$(PACKAGEVERSION)-$(PACKAGEREVISION)
PACKAGEDIR := .build/deb/$(PACKAGEFULLNAME)
DEBIANREL := forky

LIBMODULEFILES := \
	UILib/Context \
	UILib/Command \
	UILib/Pane \
	UILib/Set \
	UILib/Named \
	UILib

.build/deb/$(PACKAGEFULLNAME).deb: \
		${BINPATH}/pinafore1 \
		${BINPATH}/pinadoc \
		$(foreach I,$(LIBMODULEFILES),Pinafore/pinafore-lib-script/data/$(I).pinafore) \
		deb/copyright \
		deb/control.m4 \
		deb/changelog.m4
	rm -rf $(PACKAGEDIR)
	mkdir -p $(PACKAGEDIR)/usr/bin
	cp ${BINPATH}/pinafore1 $(PACKAGEDIR)/usr/bin/
	cp ${BINPATH}/pinadoc $(PACKAGEDIR)/usr/bin/
	mkdir -p $(PACKAGEDIR)/usr/share/pinafore/lib/UILib
	for i in $(LIBMODULEFILES); do cp Pinafore/pinafore-lib-script/data/$$i.pinafore $(PACKAGEDIR)/usr/share/pinafore/lib/$$i.pinafore; done
	mkdir -p $(PACKAGEDIR)/usr/share/doc/pinafore
	cp deb/copyright $(PACKAGEDIR)/usr/share/doc/pinafore/
	$(STACK) exec -- \
		m4 \
		-D PACKAGENAME="$(PACKAGENAME)" \
		-D PACKAGEVERSION="$(PACKAGEVERSION)" \
		-D PACKAGEREVISION="$(PACKAGEREVISION)" \
		-D DEBIANREL="$(DEBIANREL)" \
		-D RELEASEDATE="$$(date -R)" \
		deb/changelog.m4 | gzip -9 > $(PACKAGEDIR)/usr/share/doc/pinafore/changelog.Debian.gz
	mkdir -p $(PACKAGEDIR)/usr/share/bash-completion/completions/
	$(STACK) exec -- $< --bash-completion-script /usr/bin/pinafore1 > $(PACKAGEDIR)/usr/share/bash-completion/completions/pinafore
	mkdir -p $(PACKAGEDIR)/DEBIAN
	$(STACK) exec -- \
		m4 \
		-D PACKAGENAME="$(PACKAGENAME)" \
		-D PACKAGEVERSION="$(PACKAGEVERSION)" \
		-D PACKAGEREVISION="$(PACKAGEREVISION)" \
		deb/control.m4 > $(PACKAGEDIR)/DEBIAN/control
	$(STACK) exec --cwd $(PACKAGEDIR) -- md5sum $$(cd $(PACKAGEDIR) && find * -type f -not -path 'DEBIAN/*') > $(PACKAGEDIR)/DEBIAN/md5sums
	chmod -R g-w $(PACKAGEDIR)
	$(STACK) exec --cwd .build/deb -- dpkg-deb --root-owner-group --build -Zxz $(PACKAGEFULLNAME)
	$(STACK) exec -- \
		lintian \
		--tag-display-limit 0 \
		--display-info \
		--fail-on info \
		--suppress-tags-from-file deb/lintian-ignore \
		.build/deb/$(PACKAGEFULLNAME).deb

TESTDISTROS := ubuntu:26.04 debian:$(DEBIANREL)

out/pinafore.deps: ${BINPATH}/pinafore1 out
	ldd $< > $@

.PHONY: deps
deps: out/pinafore.deps

out/$(PACKAGEFULLNAME).deb: .build/deb/$(PACKAGEFULLNAME).deb deb/installtest out
	install -m 755 deb/installtest .build/deb/
	install -m 755 deb/checkscript .build/deb/
	for distro in $(TESTDISTROS); do echo DISTRO: $$distro; docker run --rm -v `pwd`/.build/deb:/home -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix:rw -it $$distro /home/installtest $(PACKAGEFULLNAME).deb `id -u` || exit $$?; done
	cp $< $@

.PHONY: deb
deb: out/$(PACKAGEFULLNAME).deb


### Nix

# Use this on a Nix system
# **/*.nix
nix-fmt:
	shopt -s globstar && nix $(NIXFLAGS) fmt *.nix

# Use this on a Nix system
nix-build-%: out
	nix $(NIXFLAGS) build .#$*

# Use this on a Nix system
nix-check: out
	nix $(NIXFLAGS) flake check .

# Use this on a Nix system
nix-flake: nix-check nix-build-vscode-extension

nix/docker/flake.nix: flake.nix
	cp $< $@

nix/docker/flake.lock: flake.lock
	cp $< $@

nix/docker/stack.yaml: stack.yaml Makefile
	$(STACK) exec -- yq '.packages=[] | del(.flags)' $< > $@

nix/docker/stack.yaml.lock: stack.yaml.lock
	cp $< $@

nix-docker-image: nix/docker/flake.nix nix/docker/flake.lock nix/docker/stack.yaml nix/docker/stack.yaml.lock
	docker build --memory 8g --memory-swap 16g -t nix-build nix/docker

nix-docker-flake: nix-docker-image
	mkdir -p nix/home
	docker run --rm -v `pwd`:/workspace -ti nix-build make nix-flake

nix-docker-shell: nix-docker-image
	mkdir -p nix/home
	docker run --rm -v `pwd`:/workspace -ti nix-build bash


### Support data

out/support:
	mkdir -p $@

out/support/syntax-data.json: ${BINPATH}/pinadata out/support
	$(STACK) exec -- $< --syntax-data > $@


### Pygments lexer

PYGLEXERVERSION := $(PINAFOREVERSIONABC)

support/pygments-lexer/pinafore_lexer/syntax-data.json: out/support/syntax-data.json
	cp $< $@

out/support/pinafore_lexer-$(PYGLEXERVERSION).tar.gz: \
 out/support \
 support/pygments-lexer/pyproject.toml \
 support/pygments-lexer/pinafore_lexer/__init__.py \
 support/pygments-lexer/pinafore_lexer/syntax-data.json
	echo $(PYGLEXERVERSION) > support/pygments-lexer/VERSION
	$(STACK) exec -- .build/python/bin/python3 -m build -o out/support/ support/pygments-lexer/

.PHONY: pyg-lexer
pyg-lexer: out/support/pinafore_lexer-$(PYGLEXERVERSION).tar.gz


### Website

LIBMODULEDOCS := \
    pinafore \
    media \
    gnome \
	UILib

support/website/library/%.md: ${BINPATH}/pinadoc
	mkdir -p support/website/library
	$(STACK) exec -- $< $(subst .,/,$*) --include Pinafore/pinafore-lib-script/data > $@

support/website/generated/infix.md: ${BINPATH}/pinadata
	mkdir -p support/website/generated
	$(STACK) exec -- $< --infix > $@

support/website/generated/type-infix.md: ${BINPATH}/pinadata
	mkdir -p support/website/generated
	$(STACK) exec -- $< --infix-type > $@

.PHONY: scour
scour: support/website/img/information.svg docker-image
	$(STACK) exec -- scour $< | $(STACK) exec -- sponge $<

support/website/generated/img/information.png: support/website/img/information.png
	mkdir -p support/website/generated/img
	cp $< $@

support/website/generated/img/logo.png: support/branding/logo.svg
	mkdir -p support/website/generated/img
	$(STACK) exec -- rsvg-convert -w 200 -h 200 $< -o $@

support/website/generated/img/favicon.png: support/branding/logo.svg
	mkdir -p support/website/generated/img
	$(STACK) exec -- rsvg-convert -w 32 -h 32 $< -o $@

%.ico: %.png
	$(STACK) exec -- convert $< -background transparent $@

.PHONY: docs
docs: \
 docker-image \
 .build/python \
 support/website/generated/img/logo.png \
 support/website/generated/img/favicon.ico \
 $(foreach f,$(LIBMODULEDOCS),support/website/library/$f.md) \
 support/website/generated/infix.md \
 support/website/generated/type-infix.md \
 support/website/generated/img/information.png \
 out/support/pinafore_lexer-$(PYGLEXERVERSION).tar.gz
	mkdir -p support/website/generated/examples
	cp Pinafore/pinafore-app/examples/* support/website/generated/examples/
	$(STACK) exec -- .build/python/bin/pip install out/support/pinafore_lexer-$(PYGLEXERVERSION).tar.gz
	rm -rf out/support/website
	mkdir -p out/support/website
	$(STACK) exec -- .build/python/bin/sphinx-build -D release="$(PINAFOREVERSION)" -D myst_substitutions.PINAFOREVERSION="$(PINAFOREVERSION)" -W --keep-going -b dirhtml support/website out/support/website/dirhtml

.PHONY: check-snippets
check-snippets: ${BINPATH}/pinafore1
	mkdir -p out/support/website/snippets
	rm -f out/support/website/snippets/*
	$(STACK) exec --docker-env BINPATH=${BINPATH} -- support/website/check-snippets

### VSCode extension

VSCXVERSION := $(PINAFOREVERSIONABC)

VSCXDIR := support/vsc-extension/vsce

$(VSCXDIR)/%.json: $(VSCXDIR)/%.yaml out/support/syntax-data.json
	$(STACK) exec -- env VSCXVERSION="$(VSCXVERSION)" yq --from-file support/vsc-extension/transform.yq -o json $< > $@

$(VSCXDIR)/images/logo.png: support/branding/logo.svg
	mkdir -p $(VSCXDIR)/images
	$(STACK) exec -- rsvg-convert -w 256 -h 256 $< -o $@

out/support/pinafore-$(VSCXVERSION).vsix: docker-image out/support \
 $(VSCXDIR)/package.json \
 $(VSCXDIR)/LICENSE \
 $(VSCXDIR)/README.md \
 $(VSCXDIR)/CHANGELOG.md \
 $(VSCXDIR)/language-configuration.json \
 $(VSCXDIR)/images/logo.png \
 $(VSCXDIR)/syntaxes/pinafore.tmLanguage.json
	cd $(VSCXDIR) && $(STACK) exec -- vsce package -o ../../../$@

.PHONY: vsc-extension
vsc-extension: out/support/pinafore-$(VSCXVERSION).vsix


### Test images

Changes/changes-gnome/examples/showImages/images/%.RGB.jpeg: Changes/changes-gnome/examples/showImages/%.jpeg
	mkdir -p Changes/changes-gnome/examples/showImages/images
	$(STACK) exec -- convert $< -colorspace RGB $@

Changes/changes-gnome/examples/showImages/images/%.YCbCr.jpeg: Changes/changes-gnome/examples/showImages/%.jpeg
	mkdir -p Changes/changes-gnome/examples/showImages/images
	$(STACK) exec -- convert $< -colorspace YCbCr $@

.PHONY: testimages
testimages: docker-image \
	Changes/changes-gnome/examples/showImages/images/cat.RGB.jpeg \
	Changes/changes-gnome/examples/showImages/images/cat.YCbCr.jpeg \
	Changes/changes-gnome/examples/showImages/images/stairs.RGB.jpeg \
	Changes/changes-gnome/examples/showImages/images/stairs.YCbCr.jpeg


### Full build, clean

.PHONY: full
full: testimages format deb nix-docker-flake deps dep-info check-snippets docs pyg-lexer vsc-extension

.PHONY: clean
clean:
	rm -rf .build
	rm -rf out
	rm -rf Changes/*/*.cabal
	rm -rf Pinafore/*/*.cabal
	rm -rf Changes/changes-gnome/examples/showImages/images
	rm -rf nix/docker/flake.nix
	rm -rf nix/docker/flake.lock
	rm -rf nix/docker/stack.yaml
	rm -rf nix/docker/stack.yaml.lock
	rm -rf support/website/generated
	rm -rf support/website/library
	rm -rf support/vsc-extension/*.json
	rm -rf support/vsc-extension/*/*.json
	rm -rf support/vsc-extension/*/*/*.json
	rm -rf support/pygments-lexer/*/*.json
	rm -rf support/pygments-lexer/*.egg-info
	rm -rf .stack-work
	rm -rf */*/.stack-work
	rm -rf */*/test/*/*.out
	rm -rf */*/test/*/*/*.out
	rm -rf result
	rm -rf .stack-root

.PHONY: update-locks
update-locks: docker-image
	rm -f stack.yaml.lock
	stack exec -- echo -n
	nix flake update


### Top

top-format:
	make format

top-watch-build:
	make watch-build

top-build:
	make exe

top-single-build:
	make single=1 exe

top-nix-flake:
	make nix-flake

top-format-test:
	make test=1 format exe

top-full:
	make haddock=1 test=1 bench=1 full

top-release:
	make haddock=1 test=1 bench=1 clean full

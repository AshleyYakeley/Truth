default: full

PINAFOREVERSION := 0.6

# must be three numbers, add .0 as necessary
PINAFOREVERSIONABC := $(PINAFOREVERSION).0

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

ifeq ($(stackroot),1)
STACKROOTFLAGS := --stack-root $$PWD/.stack-root
else
STACKROOTFLAGS :=
endif

STACKFLAGS := $(STACKROOTFLAGS) $(DOCKERFLAGS) $(JOBFLAGS) --ta --hide-successes

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

ifeq ($(nix-docker),1)
BINPATH := /binpath
else
BINPATH := $(shell stack $(STACKFLAGS) path --local-bin)
endif

### Docker image

.PHONY: docker-image
docker-image:
ifeq ($(nodocker),1)
else
	docker build -t local-build docker
endif


### Formatting

${BINPATH}/hindent: docker-image
	stack $(STACKFLAGS) install hindent

.PHONY: hindent
hindent: ${BINPATH}/hindent

.PHONY: format
format: ${BINPATH}/hindent
	env BINPATH=${BINPATH} stack --docker-env BINPATH $(STACKFLAGS) exec -- bin/hindent-all


### Licensing info

out:
	mkdir -p $@

out/licensing: out
	stack ls dependencies --license | awk '{t=$$1;$$1=$$2;$$2=t;print}' | sort > $@

.PHONY: licensing
licensing: out/licensing


### Executables

${BINPATH}/pinafore ${BINPATH}/pinadata ${BINPATH}/pinadoc &: out docker-image
ifeq ($(nodocker),1)
else
	rm -rf out/logs
ifeq ($(haddock),1)
	rm -rf out/haddock
endif
endif
	xhost +si:localuser:$${USER}
	stack --docker-env DISPLAY $(STACKFLAGS) install --test --bench $(TESTFLAGS) $(BENCHFLAGS) $(HADDOCKFLAGS)
	strip --remove-section=.comment ${BINPATH}/pinafore
	strip --remove-section=.comment ${BINPATH}/pinadoc
ifeq ($(nodocker),1)
else
ifeq ($(haddock),1)
	cp -r `stack $(STACKFLAGS) path --local-doc-root` out/haddock
endif
endif
ifeq ($(bench),1)
	test -n "$$(git status -s)" || (stack $(STACKFLAGS) exec -- benchgraph/adapters/criterion/export_benchs.sh Pinafore/pinafore-app/benchmarks.json > benchmarks/pinafore-`git rev-parse HEAD`.ndjson)
endif

.PHONY: exe
exe: ${BINPATH}/pinafore


### Debian package

PACKAGENAME := pinafore
PACKAGEVERSION := $(PINAFOREVERSION)
PACKAGEREVISION := 1
PACKAGEFULLNAME := $(PACKAGENAME)_$(PACKAGEVERSION)-$(PACKAGEREVISION)
PACKAGEDIR := .build/deb/$(PACKAGEFULLNAME)
DEBIANREL := bookworm

LIBMODULEFILES := \
	UILib/Context \
	UILib/Pane \
	UILib/Named \
	UILib

.build/deb/$(PACKAGEFULLNAME).deb: \
		${BINPATH}/pinafore \
		${BINPATH}/pinadoc \
		$(foreach I,$(LIBMODULEFILES),Pinafore/pinafore-lib-stdlib/data/$(I).pinafore) \
		deb/copyright \
		deb/control.m4 \
		deb/changelog.m4
	rm -rf $(PACKAGEDIR)
	mkdir -p $(PACKAGEDIR)/usr/bin
	cp ${BINPATH}/pinafore $(PACKAGEDIR)/usr/bin/
	cp ${BINPATH}/pinadoc $(PACKAGEDIR)/usr/bin/
	mkdir -p $(PACKAGEDIR)/usr/share/pinafore/lib/UILib
	for i in $(LIBMODULEFILES); do cp Pinafore/pinafore-lib-stdlib/data/$$i.pinafore $(PACKAGEDIR)/usr/share/pinafore/lib/$$i.pinafore; done
	mkdir -p $(PACKAGEDIR)/usr/share/doc/pinafore
	cp deb/copyright $(PACKAGEDIR)/usr/share/doc/pinafore/
	stack $(STACKFLAGS) exec -- \
		m4 \
		-D PACKAGENAME="$(PACKAGENAME)" \
		-D PACKAGEVERSION="$(PACKAGEVERSION)" \
		-D PACKAGEREVISION="$(PACKAGEREVISION)" \
		-D DEBIANREL="$(DEBIANREL)" \
		-D RELEASEDATE="$$(date -R)" \
		deb/changelog.m4 | gzip -9 > $(PACKAGEDIR)/usr/share/doc/pinafore/changelog.Debian.gz
	mkdir -p $(PACKAGEDIR)/usr/share/bash-completion/completions/
	stack $(STACKFLAGS) exec -- $< --bash-completion-script /usr/bin/pinafore > $(PACKAGEDIR)/usr/share/bash-completion/completions/pinafore
	mkdir -p $(PACKAGEDIR)/DEBIAN
	stack $(STACKFLAGS) exec -- \
		m4 \
		-D PACKAGENAME="$(PACKAGENAME)" \
		-D PACKAGEVERSION="$(PACKAGEVERSION)" \
		-D PACKAGEREVISION="$(PACKAGEREVISION)" \
		deb/control.m4 > $(PACKAGEDIR)/DEBIAN/control
	stack $(STACKFLAGS) exec --cwd $(PACKAGEDIR) -- md5sum $$(cd $(PACKAGEDIR) && find * -type f -not -path 'DEBIAN/*') > $(PACKAGEDIR)/DEBIAN/md5sums
	chmod -R g-w $(PACKAGEDIR)
	stack $(STACKFLAGS) exec --cwd .build/deb -- dpkg-deb --root-owner-group --build -Zxz $(PACKAGEFULLNAME)
	stack $(STACKFLAGS) exec -- \
		lintian \
		--tag-display-limit 0 \
		--display-info \
		--fail-on info \
		--suppress-tags-from-file deb/lintian-ignore \
		.build/deb/$(PACKAGEFULLNAME).deb

TESTDISTROS := ubuntu:22.04 bitnami/minideb:$(DEBIANREL)

out/pinafore.deps: ${BINPATH}/pinafore out
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
nix-flake: out
	nix flake check .?submodules=1
	nix build .?submodules=1#vscode-extension

nix/docker/flake.nix: flake.nix
	cp $< $@

nix/docker/flake.lock: flake.lock
	cp $< $@

nix/docker/stack.yaml: stack.yaml
	stack $(STACKFLAGS) exec -- yq '.packages=[]' $< > $@

nix/docker/stack.yaml.lock: stack.yaml.lock
	cp $< $@

nix-docker-image: nix/docker/flake.nix nix/docker/flake.lock nix/docker/stack.yaml nix/docker/stack.yaml.lock
	docker build -t nix-build nix/docker

nix-docker-flake: nix-docker-image
	mkdir -p nix/home
	docker run --rm -v `pwd`:/workspace -ti nix-build make nix-docker=1 nix-flake

nix-docker-shell: nix-docker-image
	mkdir -p nix/home
	docker run --rm -v `pwd`:/workspace -ti nix-build bash


### Support data

out/support:
	mkdir -p $@

out/support/syntax-data.json: ${BINPATH}/pinadata out/support
	stack $(STACKFLAGS) exec -- $< --syntax-data > $@


### Pygments lexer

PYGLEXERVERSION := $(PINAFOREVERSIONABC)

support/pygments-lexer/pinafore_lexer/syntax-data.json: out/support/syntax-data.json
	cp $< $@

out/support/pinafore_lexer-$(PYGLEXERVERSION).tar.gz: \
 out/support \
 support/pygments-lexer/pyproject.toml \
 support/pygments-lexer/pinafore_lexer/__init__.py \
 support/pygments-lexer/pinafore_lexer/syntax-data.json
	stack $(STACKFLAGS) exec -- env PYGLEXERVERSION="$(PYGLEXERVERSION)" python3 -m build -o out/support/ support/pygments-lexer/

.PHONY: pyg-lexer
pyg-lexer: out/support/pinafore_lexer-$(PYGLEXERVERSION).tar.gz


### Website

LIBMODULEDOCS := \
    pinafore \
    media \
    gnome \
	UILib

doc/library/%.md: ${BINPATH}/pinadoc
	mkdir -p doc/library
	stack $(STACKFLAGS) exec -- $< $(subst .,/,$*) --include Pinafore/pinafore-lib-stdlib/data > $@

doc/generated/infix.md: ${BINPATH}/pinadata
	mkdir -p doc/generated
	stack $(STACKFLAGS) exec -- $< --infix > $@

doc/generated/type-infix.md: ${BINPATH}/pinadata
	mkdir -p doc/generated
	stack $(STACKFLAGS) exec -- $< --infix-type > $@

.PHONY: scour
scour: doc/img/information.svg docker-image
	stack $(STACKFLAGS) exec -- scour $< | stack $(STACKFLAGS) exec -- sponge $<

doc/generated/img/information.png: doc/img/information.png
	mkdir -p doc/generated/img
	cp $< $@

.PHONY: docs
docs: \
 $(foreach f,$(LIBMODULEDOCS),doc/library/$f.md) \
 doc/generated/infix.md \
 doc/generated/type-infix.md \
 doc/generated/img/information.png \
 out/support/pinafore_lexer-$(PYGLEXERVERSION).tar.gz \
 docker-image
	mkdir -p doc/generated/examples
	cp Pinafore/pinafore-app/examples/* doc/generated/examples/
	stack $(STACKFLAGS) exec -- pip3 install --user out/support/pinafore_lexer-$(PYGLEXERVERSION).tar.gz
	rm -rf out/doc
	mkdir -p out/doc
	stack $(STACKFLAGS) exec -- sphinx-build -D release="$(PINAFOREVERSION)" -D myst_substitutions.PINAFOREVERSION="$(PINAFOREVERSION)" -W --keep-going -b dirhtml doc out/doc/dirhtml


### VSCode extension

VSCXVERSION := $(PINAFOREVERSIONABC)

support/vsc-extension/vsce/%.json: support/vsc-extension/vsce/%.yaml out/support/syntax-data.json
	stack $(STACKFLAGS) exec -- env VSCXVERSION="$(VSCXVERSION)" yq --from-file support/vsc-extension/transform.yq -o json $< > $@

out/support/pinafore-$(VSCXVERSION).vsix: docker-image out/support \
 support/vsc-extension/vsce/package.json \
 support/vsc-extension/vsce/LICENSE \
 support/vsc-extension/vsce/README.md \
 support/vsc-extension/vsce/CHANGELOG.md \
 support/vsc-extension/vsce/language-configuration.json \
 support/vsc-extension/vsce/syntaxes/pinafore.tmLanguage.json
	cd support/vsc-extension/vsce && stack $(STACKFLAGS) exec -- vsce package -o ../../../$@

.PHONY: vsc-extension
vsc-extension: out/support/pinafore-$(VSCXVERSION).vsix


### Test images

Changes/changes-gnome/examples/showImages/images/%.RGB.jpeg: Changes/changes-gnome/examples/showImages/%.jpeg
	mkdir -p Changes/changes-gnome/examples/showImages/images
	stack $(STACKFLAGS) exec -- convert $< -colorspace RGB $@

Changes/changes-gnome/examples/showImages/images/%.YCbCr.jpeg: Changes/changes-gnome/examples/showImages/%.jpeg
	mkdir -p Changes/changes-gnome/examples/showImages/images
	stack $(STACKFLAGS) exec -- convert $< -colorspace YCbCr $@

.PHONY: testimages
testimages: docker-image \
	Changes/changes-gnome/examples/showImages/images/cat.RGB.jpeg \
	Changes/changes-gnome/examples/showImages/images/cat.YCbCr.jpeg \
	Changes/changes-gnome/examples/showImages/images/stairs.RGB.jpeg \
	Changes/changes-gnome/examples/showImages/images/stairs.YCbCr.jpeg


### Full build, clean

.PHONY: full
full: testimages format deb nix-docker-flake deps licensing docs pyg-lexer vsc-extension

.PHONY: clean
clean:
	rm -rf .build
	rm -rf out
	rm -rf doc/generated
	rm -rf doc/library
	rm -rf Changes/changes-gnome/examples/showImages/images
	rm -rf nix/docker/flake.nix
	rm -rf nix/docker/flake.lock
	rm -rf nix/docker/stack.yaml
	rm -rf nix/docker/stack.yaml.lock
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
ifeq ($(stackroot),1)
	rm -rf .stack-root
endif

.PHONY: update-locks
update-locks:
	rm -f stack.yaml.lock
	stack exec -- echo -n
	rm -f flake.lock
	nix flake lock


### Top

top-format:
	make format; bin/reportstatus $$?

top-build:
	make exe; bin/reportstatus $$?

top-single-build:
	make single=1 exe; bin/reportstatus $$?

top-format-test:
	make test=1 format exe; bin/reportstatus $$?

top-full-resume:
	make haddock=1 test=1 bench=1 full; bin/reportstatus $$?

top-full:
	make haddock=1 test=1 bench=1 clean full; bin/reportstatus $$?

top-release:
	make stackroot=1 haddock=1 test=1 bench=1 clean full; bin/reportstatus $$?

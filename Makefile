default: full


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

BINPATH := $(shell stack $(STACKFLAGS) path --local-bin)


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

${BINPATH}/pinafore ${BINPATH}/pinafore-doc &: out docker-image
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
ifeq ($(test),1)
	stack $(STACKFLAGS) exec -- ${BINPATH}/pinafore-doc --include Pinafore/pinafore-app/test/pinafore-doc --module test > Pinafore/pinafore-app/test/pinafore-doc/test.out.md
	diff -u Pinafore/pinafore-app/test/pinafore-doc/test.ref.md Pinafore/pinafore-app/test/pinafore-doc/test.out.md
endif
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
PACKAGEVERSION := 0.4.1
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
		$(foreach I,$(LIBMODULEFILES),Pinafore/pinafore-stdlib/data/$(I).pinafore) \
		deb/copyright \
		deb/control.m4 \
		deb/changelog.m4
	rm -rf $(PACKAGEDIR)
	mkdir -p $(PACKAGEDIR)/usr/bin
	cp ${BINPATH}/pinafore $(PACKAGEDIR)/usr/bin/
	mkdir -p $(PACKAGEDIR)/usr/share/pinafore/lib/UILib
	for i in $(LIBMODULEFILES); do cp Pinafore/pinafore-stdlib/data/$$i.pinafore $(PACKAGEDIR)/usr/share/pinafore/lib/$$i.pinafore; done
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

nix-docker-image:
	docker build -t nix-build nix/docker

nix-docker-flake: nix-docker-image
	mkdir -p nix/home
	docker run --rm -v `pwd`:/workspace -ti nix-build nix flake check .?submodules=1


### Support data

out/support:
	mkdir -p $@

out/support/syntax-data.json: ${BINPATH}/pinafore-doc out/support
	stack $(STACKFLAGS) exec -- $< --syntax-data > $@


### Pygments lexer

PYGLEXERVERSION := $(PACKAGEVERSION).0

support/pygments-lexer/pinafore_lexer/syntax-data.json: out/support/syntax-data.json
	cp $< $@

out/support/pinafore-lexer-$(PYGLEXERVERSION).tar.gz: \
 out/support \
 support/pygments-lexer/pyproject.toml \
 support/pygments-lexer/pinafore_lexer/__init__.py \
 support/pygments-lexer/pinafore_lexer/syntax-data.json
	stack $(STACKFLAGS) exec -- python3 -m build -o out/support/ support/pygments-lexer/

.PHONY: pyg-lexer
pyg-lexer: out/support/pinafore-lexer-$(PYGLEXERVERSION).tar.gz


### Website

LIBMODULEDOCS := \
    pinafore \
    pinafore-media \
    pinafore-gnome \
	UILib

mkdocs/docs/library/%.md: ${BINPATH}/pinafore-doc
	mkdir -p mkdocs/docs/library
	stack $(STACKFLAGS) exec -- $< --module $(subst .,/,$*) --include Pinafore/pinafore-stdlib/data > $@

mkdocs/generated/infix.md: ${BINPATH}/pinafore-doc
	mkdir -p mkdocs/generated
	stack $(STACKFLAGS) exec -- $< --infix > $@

mkdocs/generated/type-infix.md: ${BINPATH}/pinafore-doc
	mkdir -p mkdocs/generated
	stack $(STACKFLAGS) exec -- $< --infix-type > $@

.PHONY: scour
scour: mkdocs/docs/img/information.svg docker-image
	stack $(STACKFLAGS) exec -- scour $< | stack $(STACKFLAGS) exec -- sponge $<

mkdocs/generated/img/information.png: mkdocs/docs/img/information.png
	mkdir -p mkdocs/generated/img
	cp $< $@

.PHONY: docs
docs: \
 $(foreach f,$(LIBMODULEDOCS),mkdocs/docs/library/$f.md) \
 mkdocs/generated/infix.md \
 mkdocs/generated/type-infix.md \
 mkdocs/generated/img/information.png \
 out/support/pinafore-lexer-$(PYGLEXERVERSION).tar.gz \
 docker-image
	mkdir -p mkdocs/generated/examples
	cp Pinafore/pinafore-app/examples/* mkdocs/generated/examples/
	stack $(STACKFLAGS) exec -- pip3 install --user out/support/pinafore-lexer-$(PYGLEXERVERSION).tar.gz
	mkdir -p out/website
	stack $(STACKFLAGS) exec --cwd mkdocs -- mkdocs build --site-dir ../out/website


### VSCode extension

VSCXVERSION := $(PACKAGEVERSION).0

support/vsc-extension/vsce/%.json: support/vsc-extension/vsce/%.yaml out/support/syntax-data.json
	stack $(STACKFLAGS) exec -- yq --from-file support/vsc-extension/transform.yq -o json $< > $@

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
full: testimages format deb nix-docker-flake deps licensing docs vsc-extension

.PHONY: clean
clean:
	rm -rf .build
	rm -rf out
	rm -rf mkdocs/generated
	rm -rf mkdocs/docs/library
	rm -rf Changes/changes-gnome/examples/showImages/images
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

.PHONY: full-clean
full-clean: clean
	rm -rf .stack-root

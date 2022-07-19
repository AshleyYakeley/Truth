default: full

ifeq ($(nodocker),1)
DOCKERFLAGS := --no-docker --ta --hide-successes
else
DOCKERFLAGS := --no-interleaved-output
endif

ifeq ($(single),1)
JOBFLAGS := --no-keep-going --jobs 1
else
JOBFLAGS := --keep-going
endif

STACKFLAGS := $(DOCKERFLAGS) $(JOBFLAGS)

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


.PHONY: docker-image

docker-image:
ifeq ($(nodocker),1)
else
	docker build -t local-build docker
endif

${BINPATH}/hindent: docker-image
	stack $(STACKFLAGS) install hindent

.PHONY: hindent

hindent: ${BINPATH}/hindent

.PHONY: format

format: ${BINPATH}/hindent
	for f in `find -name '*.hs' -not -path '*.stack-work/*' | grep -v -e -- -f .hindent.ignore`; do ${BINPATH}/hindent $$f || exit; done

${BINPATH}/licensor: docker-image
	stack $(STACKFLAGS) install licensor

out:
	mkdir -p out

out/licensing: ${BINPATH}/licensor out
	$< --quiet > $@

.PHONY: licensing

licensing: out/licensing

${BINPATH}/pinafore ${BINPATH}/pinafore-doc &: out docker-image
	rm -rf .stack-work/logs
ifeq ($(nodocker),1)
else
	rm -rf out/logs
ifeq ($(haddock),1)
	rm -rf out/haddock
endif
endif
	stack --docker-env DISPLAY $(STACKFLAGS) install --test --bench $(TESTFLAGS) $(BENCHFLAGS) $(HADDOCKFLAGS) || (test -d .stack-work/logs && cp -r .stack-work/logs out/; exit 1)
ifeq ($(nodocker),1)
else
	cp -r .stack-work/logs out/
ifeq ($(haddock),1)
	cp -r `stack $(STACKFLAGS) path --local-doc-root` out/haddock
endif
endif
ifeq ($(bench),1)
	test -n "$$(git status -s)" || (stack $(STACKFLAGS) exec -- benchgraph/adapters/criterion/export_benchs.sh Pinafore/pinafore-app/benchmarks.json > benchmarks/pinafore-`git rev-parse HEAD`.ndjson)
endif

.PHONY: exe

exe: ${BINPATH}/pinafore

PACKAGENAME := pinafore
PACKAGEVERSION := 0.4
PACKAGEREVISION := 1
PACKAGEFULLNAME := $(PACKAGENAME)_$(PACKAGEVERSION)-$(PACKAGEREVISION)
PACKAGEDIR := .build/deb/$(PACKAGEFULLNAME)
DEBIANREL := buster

.build/deb/$(PACKAGEFULLNAME).deb: \
		${BINPATH}/pinafore \
		Pinafore/lib/UIStuff/Selection.pinafore \
		Pinafore/lib/UIStuff/Named.pinafore \
		deb/copyright \
		deb/control.m4 \
		deb/changelog.m4
	rm -rf $(PACKAGEDIR)
	mkdir -p $(PACKAGEDIR)/usr/bin
	cp ${BINPATH}/pinafore $(PACKAGEDIR)/usr/bin/
	mkdir -p $(PACKAGEDIR)/usr/share/pinafore/lib/UIStuff
	cp Pinafore/lib/UIStuff/Selection.pinafore $(PACKAGEDIR)/usr/share/pinafore/lib/UIStuff/
	cp Pinafore/lib/UIStuff/Named.pinafore $(PACKAGEDIR)/usr/share/pinafore/lib/UIStuff/
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
	$< --bash-completion-script /usr/bin/pinafore > $(PACKAGEDIR)/usr/share/bash-completion/completions/pinafore
	mkdir -p $(PACKAGEDIR)/DEBIAN
	stack $(STACKFLAGS) exec -- \
		m4 \
		-D PACKAGENAME="$(PACKAGENAME)" \
		-D PACKAGEVERSION="$(PACKAGEVERSION)" \
		-D PACKAGEREVISION="$(PACKAGEREVISION)" \
		deb/control.m4 > $(PACKAGEDIR)/DEBIAN/control
	chmod -R g-w $(PACKAGEDIR)
	stack $(STACKFLAGS) exec --cwd .build/deb -- dpkg-deb --root-owner-group --build $(PACKAGEFULLNAME)
	stack $(STACKFLAGS) exec -- lintian --fail-on-warnings --suppress-tags-from-file deb/lintian-ignore .build/deb/$(PACKAGEFULLNAME).deb

TESTDISTROS := ubuntu:18.04 ubuntu:21.04 bitnami/minideb:$(DEBIANREL)

out/pinafore.deps: ${BINPATH}/pinafore out
	ldd $< > $@

.PHONY: deps

deps: out/pinafore.deps

out/$(PACKAGEFULLNAME).deb: .build/deb/$(PACKAGEFULLNAME).deb deb/installtest out
	install -m 755 deb/installtest .build/deb/
	install -m 755 deb/checkscript .build/deb/
	for distro in $(TESTDISTROS); do docker run --rm -v `pwd`/.build/deb:/home -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix:rw -it $$distro /home/installtest $(PACKAGEFULLNAME).deb `id -u`; done
	cp $< $@

.PHONY: deb

deb: out/$(PACKAGEFULLNAME).deb

LIBMODULES := \
    Std \
    Colour \
    Cairo \
    Image \
    GIO \
    GTK \
    Debug \
    Debug.GTK \
	UIStuff.Selection \
	UIStuff.Named

mkdocs/docs/library/%.md: ${BINPATH}/pinafore-doc
	mkdir -p mkdocs/docs/library
	$< --module $* --include Pinafore/lib > $@

mkdocs/generated/infix.md: ${BINPATH}/pinafore-doc
	mkdir -p mkdocs/generated
	$< --infix > $@

mkdocs/generated/type-infix.md: ${BINPATH}/pinafore-doc
	mkdir -p mkdocs/generated
	$< --infix-type > $@

.PHONY: scour

scour: mkdocs/docs/img/information.svg docker-image
	stack $(STACKFLAGS) exec -- scour $< | stack $(STACKFLAGS) exec -- sponge $<

mkdocs/generated/img/information.png: mkdocs/docs/img/information.png
	mkdir -p mkdocs/generated/img
	cp $< $@

.PHONY: docs

docs: $(foreach f,$(LIBMODULES),mkdocs/docs/library/$f.md) mkdocs/generated/infix.md mkdocs/generated/type-infix.md mkdocs/generated/img/information.png docker-image
	mkdir -p mkdocs/generated/examples
	cp Pinafore/pinafore-app/examples/* mkdocs/generated/examples/
	stack $(STACKFLAGS) exec -- pip3 install --user file://`pwd`/support/pygments-lexer/
	mkdir -p out/website
	stack $(STACKFLAGS) exec --cwd mkdocs -- mkdocs build --site-dir ../out/website

VSCXVERSION := $(PACKAGEVERSION).0

out/pinafore-$(VSCXVERSION).vsix: docker-image out \
 support/vsc-extension/package.json \
 support/vsc-extension/LICENSE \
 support/vsc-extension/README.md \
 support/vsc-extension/CHANGELOG.md \
 support/vsc-extension/language-configuration.json \
 support/vsc-extension/syntaxes/pinafore.tmLanguage.json
	cd support/vsc-extension && stack $(STACKFLAGS) exec -- vsce package -o ../../$@

.PHONY: vsc-extension

vsc-extension: out/pinafore-$(VSCXVERSION).vsix

Changes/changes-gtk/examples/showImages/images/%.RGB.jpeg: Changes/changes-gtk/examples/showImages/%.jpeg
	mkdir -p Changes/changes-gtk/examples/showImages/images
	stack $(STACKFLAGS) exec -- convert $< -colorspace RGB $@

Changes/changes-gtk/examples/showImages/images/%.YCbCr.jpeg: Changes/changes-gtk/examples/showImages/%.jpeg
	mkdir -p Changes/changes-gtk/examples/showImages/images
	stack $(STACKFLAGS) exec -- convert $< -colorspace YCbCr $@

.PHONY: testimages

testimages: docker-image \
	Changes/changes-gtk/examples/showImages/images/cat.RGB.jpeg \
	Changes/changes-gtk/examples/showImages/images/cat.YCbCr.jpeg \
	Changes/changes-gtk/examples/showImages/images/stairs.RGB.jpeg \
	Changes/changes-gtk/examples/showImages/images/stairs.YCbCr.jpeg

.PHONY: full

full: testimages format deb deps licensing docs vsc-extension

clean:
	rm -rf .build
	rm -rf out
	rm -rf mkdocs/generated
	rm -rf mkdocs/docs/library
	rm -rf Changes/changes-gtk/examples/showImages/images
	stack $(STACKFLAGS) clean

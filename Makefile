default: full

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

STACKFLAGS := $(DOCKERFLAGS) $(JOBFLAGS)

ifeq ($(test),1)
TESTFLAGS := --ta --hide-successes
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
	for f in `find -name '*.hs' -not -path '*.stack-work/*' | grep -xvf .hindent.ignore`; do ${BINPATH}/hindent $$f || exit; done

${BINPATH}/licensor: docker-image
	stack $(STACKFLAGS) install licensor

out/licensing: ${BINPATH}/licensor
	mkdir -p out
	$< --quiet > $@

.PHONY: licensing

licensing: out/licensing

${BINPATH}/pinafore: docker-image
	stack --docker-env DISPLAY $(STACKFLAGS) install --test --bench $(TESTFLAGS) $(BENCHFLAGS) $(HADDOCKFLAGS) > out/stack.out
ifeq ($(bench),1)
	test -n "$$(git status -s)" || (stack $(STACKFLAGS) exec -- benchgraph/adapters/criterion/export_benchs.sh pinafore/benchmarks.json > benchmarks/pinafore-`git rev-parse HEAD`.ndjson)
endif

.PHONY: exe

exe: ${BINPATH}/pinafore

PACKAGENAME := pinafore
PACKAGEVERSION := 0.1
PACKAGEREVISION := 1
PACKAGEFULLNAME := $(PACKAGENAME)_$(PACKAGEVERSION)-$(PACKAGEREVISION)
PACKAGEDIR := .build/deb/$(PACKAGEFULLNAME)

.build/deb/$(PACKAGEFULLNAME).deb: ${BINPATH}/pinafore deb/control.m4
	rm -rf $(PACKAGEDIR)
	mkdir -p $(PACKAGEDIR)/usr/bin
	cp ${BINPATH}/pinafore $(PACKAGEDIR)/usr/bin/
	mkdir -p $(PACKAGEDIR)/usr/share/doc/pinafore
	cp deb/copyright $(PACKAGEDIR)/usr/share/doc/pinafore/
	mkdir -p $(PACKAGEDIR)/DEBIAN
	stack $(STACKFLAGS) exec -- m4 -D PACKAGENAME="$(PACKAGENAME)" -D PACKAGEVERSION="$(PACKAGEVERSION)" -D PACKAGEREVISION="$(PACKAGEREVISION)" deb/control.m4 > $(PACKAGEDIR)/DEBIAN/control
	chmod -R g-w $(PACKAGEDIR)
	stack $(STACKFLAGS) exec --cwd .build/deb -- dpkg-deb --root-owner-group --build $(PACKAGEFULLNAME)
	stack $(STACKFLAGS) exec -- lintian --fail-on-warnings --suppress-tags-from-file deb/lintian-ignore .build/deb/$(PACKAGEFULLNAME).deb

out/$(PACKAGEFULLNAME).deb: .build/deb/$(PACKAGEFULLNAME).deb deb/installtest
	install -m 755 deb/installtest .build/deb/
	docker run --rm -v `pwd`/.build/deb:/home -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix:rw -it bitnami/minideb:latest /home/installtest `id -u`
	mkdir -p out
	cp $< $@

.PHONY: deb

deb: out/$(PACKAGEFULLNAME).deb

mkdocs/generated/predefined.md: ${BINPATH}/pinafore
	mkdir -p mkdocs/generated
	$< --doc-predefined > $@

mkdocs/generated/infix.md: ${BINPATH}/pinafore
	mkdir -p mkdocs/generated
	$< --doc-infix > $@

.PHONY: docs

docs: mkdocs/generated/predefined.md mkdocs/generated/infix.md docker-image
	mkdir -p mkdocs/generated/examples
	cp pinafore/examples/* mkdocs/generated/examples/
	stack $(STACKFLAGS) exec -- pip3 install --user file://`pwd`/support/pygments-lexer/
	stack $(STACKFLAGS) exec --cwd mkdocs -- mkdocs build

full: format deb licensing docs

.PHONY: install

install: out/$(PACKAGEFULLNAME).deb
	sudo dpkg --install $<

.PHONY: install-vsc-extension

install-vsc-extension:
	rm -rf ${HOME}/.vscode/extensions/ashleyyakeley.pinafore-0.0.1
	cp -a support/vsc-extension ${HOME}/.vscode/extensions/ashleyyakeley.pinafore-0.0.1

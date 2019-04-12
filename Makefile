default: build

ifeq ($(nodocker),1)
STACKFLAGS := --no-docker
else
STACKFLAGS :=
endif

ifeq ($(test),1)
TESTFLAGS :=
else
TESTFLAGS := --no-run-tests
endif

ifeq ($(bench),1)
BENCHFLAGS :=
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
	stack $(STACKFLAGS) install hindent

.PHONY: format

format: ${BINPATH}/hindent
	#for f in `find -name '*.hs' -not -path '*.stack-work/*' | grep -xvf .hindent.ignore`; do ${BINPATH}/hindent $$f; done

${BINPATH}/pinafore: docker-image
	stack $(STACKFLAGS) install --test --bench $(TESTFLAGS) $(BENCHFLAGS) $(HADDOCKFLAGS)

.PHONY: build

build: ${BINPATH}/pinafore

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

fullbuild: format build docs

.PHONY: install

install:
	sudo install -m 755 --strip ${BINPATH}/pinafore /usr/local/bin/pinafore

.PHONY: install-vsc-extension

install-vsc-extension:
	rm -rf ${HOME}/.vscode/extensions/ashleyyakeley.pinafore-0.0.1
	cp -a support/vsc-extension ${HOME}/.vscode/extensions/ashleyyakeley.pinafore-0.0.1

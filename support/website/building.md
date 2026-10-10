# Building

The Nix flake supports `x86_64-linux`. The Stack build also targets 64-bit Linux.

## Fetch the Source

Clone the repository with its submodules:

```text
git clone --recurse-submodules https://github.com/AshleyYakeley/Truth.git
cd Truth
```

For an existing checkout, initialise any missing submodules with `git submodule update --init --recursive`.

## Nix

With Nix and flakes enabled, build the interpreter, script libraries, and documentation generator:

```text
nix build .#pinafore
```

The executables are `result/bin/pinafore1` and `result/bin/pinadoc`.
You can also run directly from the checkout:

```text
nix run . -- path/to/file.pinafore
nix run . -- -n path/to/file.pinafore
nix run . -- -i
```

The second command parses and type-checks the script without running it.

Other useful build and validation commands:

```text
nix build .#pinafore-app:exe:pinafore1
nix build .#vscode-extension
nix flake check .
```

The first command builds just the interpreter component; `.#pinafore` also includes the script libraries and `pinadoc`.
Docker is not needed for these Nix commands.

## Stack and Docker

Install Docker and Stack, and ensure your user can run Docker.
The repository's Dockerfile supplies the native libraries and build tools needed by Stack.
Build and install the executables into `.build/bin` with:

```text
make exe
```

To enable the test suites:

```text
make test=1 exe
```

On a native system with the dependencies from `docker/Dockerfile` installed, use `make nodocker=1 exe` to skip Docker.
For local script runs, `bin/testpinafore path/to/file.pinafore` adds the source library directory and uses `test/pinafore` for local data.
Use `bin/testpinafore --build path/to/file.pinafore` to build first.

## Debian Package and Website

With the Stack/Docker build environment available:

```text
make deb
make docs
make check-snippets
```

`make deb` builds and validates a Debian package in `out/` using Docker.
`make docs` generates the library reference, syntax tables, and example sources, then builds the website in `out/support/website/dirhtml/`.
`make check-snippets` parses and type-checks the marked Pinafore snippets in the website's top-level Markdown files.

The default `make` target runs the full release workflow, including formatting, Debian packaging, Nix builds, and documentation generation.
Use the individual targets above for development.

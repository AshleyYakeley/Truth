# Repository Guidelines

## Project Structure & Module Organization

This repository contains two related Haskell codebases: `Changes/` and `Pinafore/`. Each package has `package.yaml`, generated `.cabal` metadata, source under `lib/`, `src/`, `app/`, or `data/`, and tests under `test/`. Examples live in paths such as `Pinafore/pinafore-app/examples/` and `Changes/changes-gnome/examples/`. Build and release support is in `Makefile`, `flake.nix`, `stack.yaml`, `bin/`, `deb/`, and `support/`.

## Build, Test, and Development Commands

There are two supported build paths.

Nix is preferred for agents on NixOS sandboxes where Docker is unavailable:

- `nix flake check .`: evaluate and run flake checks.
- `nix build .#pinafore`: build the default Pinafore package.
- `nix build .#vscode-extension`: build the VSCode extension output.
- `nix build .#pinafore-app:exe:pinafore1`: build the main executable directly.

The Ubuntu/Stack path expects Docker or a native Ubuntu-like system with required libraries:

- `make exe`: build and install `pinafore1`, `pinadata`, and `pinadoc` through Stack.
- `make test=1 exe`: build with test suites enabled.
- `make watch-build`: run a fast Stack file-watch build.
- `make deb`: build and validate the Debian package; this is Docker-dependent.
- `bin/testpinafore --build`: build the Pinafore executable for local script runs.

Pass `nodocker=1` to supported Make targets to skip Docker, for example `make nodocker=1 exe`.

## Running Pinafore Files

Run a Pinafore script directly with Nix:

- `nix run . -- path/to/file.pinafore`: run the file.
- `nix run . -- -n path/to/file.pinafore`: parse and type-check without running.
- `nix run . -- --data test/pinafore path/to/file.pinafore`: use a throwaway local data directory.
- `nix run . -- -i`: start interactive mode.

With Stack, use `bin/testpinafore path/to/file.pinafore`; it adds the local library include path and defaults data to `test/pinafore`.

## Coding Style & Naming Conventions

Use `make format` to run Fourmolu. Haskell modules use `UpperCamelCase` and match their path, for example `Pinafore/Library/GTK/Widget.hs`. Package directories and Cabal package names use lower-case hyphenated names, such as `pinafore-lib-gnome`. Prefer editing `package.yaml`; `.cabal` files are generated metadata.

## Testing Guidelines

Tests use `tasty` with HUnit, golden tests, and QuickCheck in selected packages. Keep tests near the package they cover, usually in `test/`, with golden files in `test/golden/` or package-specific `test/*.ref` paths. For broad validation, run `nix flake check .`; for Stack validation, use `make test=1 exe` or targeted `stack test <package>:test`.

## Agent-Specific Instructions

Assume Docker is unavailable unless told otherwise. Prefer direct Nix commands, and avoid Docker-backed Make targets in sandboxes. Leave `NIXFLAGS` unset unless debugging a local Nix configuration. Do not remove generated outputs or staged user changes unless explicitly asked.

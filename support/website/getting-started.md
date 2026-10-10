# Getting Started

## Installation

### Debian

The Debian package is built for 64-bit Linux (`amd64`).
The current packaging configuration targets Debian forky and Ubuntu 26.04; older distributions may not provide the required GTK 4 and WebKitGTK 6 libraries.
[The release page](https://github.com/AshleyYakeley/Truth/releases) includes:

* a Debian package for the Pinafore interpreter
* an extension for Visual Studio Code
* a Pygments lexer

Installing the Debian package will put the interpreter in `/usr/bin/pinafore1`.

### Nix

For Nix, use this flake: {{ "`" + "git+https://github.com/AshleyYakeley/Truth?tag=v" + PINAFOREVERSION + "`" }}. Use these outputs:

* `packages.x86_64-linux.default`: package containing the Pinafore interpreter (`pinafore1`), libraries, and documentation generator (`pinadoc`)
* `packages.x86_64-linux.vscode-extension`: an extension for Visual Studio Code, suitable for adding to `programs.vscode.extensions` in your Home Manager configuration
* `apps.x86_64-linux.default`: the Pinafore interpreter
* `apps.x86_64-linux.pinafore`: the Pinafore interpreter
* `apps.x86_64-linux.pinafore1`: the Pinafore interpreter
* `apps.x86_64-linux.pinadata`: the syntax-data generator
* `apps.x86_64-linux.pinadoc`: the Pinafore documentation generator
* `files.x86_64-linux.vscode-extension`: the extension as a VSIX file, you will have to rename the result file link to have a `.vsix` extension to install it.

## Running

Try running one of the example files. Copy/paste [this example](examples/contacts.md) to `contacts` and run this:

```text
chmod 755 contacts
./contacts
```

Alternatively, if you're curious about the type system, try running in [interactive mode](invocation.md#interactive-mode).

```text
pinafore1 -i
```

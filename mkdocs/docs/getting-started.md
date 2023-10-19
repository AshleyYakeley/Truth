# Getting Started

## Installation

### Debian

Pinafore is available for 64-bit Ubuntu 22.04 and later, and Debian 12 and later.
[The release page](https://github.com/AshleyYakeley/Truth/releases) includes:

* a Debian package for the Pinafore interpreter
* an extension for Visual Studio Code
* a Pygments lexer

Installing the Debian package will put the intepreter in `/usr/bin/pinafore`.

### Nix

For Nix, use this flake: `git+https://github.com/AshleyYakeley/Truth?tag=v0.4.1`. Use these package outputs:

* `default` or `pinafore`: the Pinafore interpreter
* `vscode-extension`: an extension for Visual Studio Code, suitable for adding to `programs.vscode.extensions` in your Home Manager configuration
* `vscode-extension-file`: the extension as a VSIX file, you will have to rename the result file link to have a `.vsix` extension to install it.

## Running

Try running one of the example files. Copy/paste [this example](examples/contacts.md) to `contacts` and run this:

    :::text
    chmod 755 contacts
    ./contacts

Alternatively, if you're curious about the type system, try running in [interactive mode](invocation.md#interactive-mode).

    :::text
    pinafore -i

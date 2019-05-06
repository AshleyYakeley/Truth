# Getting Started

## Installation

You will need a 64-bit Linux machine.

1. Install Docker (if you haven't already).
   
        :::text
        sudo apt install docker.io  
        sudo adduser $USER docker

    You will have to log out of your desktop session for `adduser` to take effect.

1. Install Stack.

    If you don't already have stack, install it like this:

        :::text
        wget -qO- https://get.haskellstack.org/ | sh

    If you do have it, make sure it's the latest version:

        :::text
        stack upgrade

1. Fetch the code.

        :::text
        git clone --recurse-submodules https://github.com/AshleyYakeley/Truth.git
        cd Truth

1. Build. This will take about an hour or so.

        :::text
        make
        make install

    (Don't do "sudo make install", that will confuse stack.)

    This will put the executable program in `/usr/local/bin/pinafore`.

## Running

Try running one of the example files:

    :::text
    pinafore pinafore/examples/people.pinafore

Alternatively, if you're curious about the type system, try running in [interactive mode](invocation.md#interactive-mode).

    :::text
    pinafore -i

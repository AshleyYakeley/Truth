# Building

You will need a 64-bit Linux machine.

1. Install Docker (if you haven't already).
   
        sudo apt install docker.io  
        sudo adduser $USER docker

    You will have to log out of your desktop session for `adduser` to take effect.

2. Install Stack.

    If you don't already have stack, install it like this:

        wget -qO- https://get.haskellstack.org/ | sh

    If you do have it, make sure it's the latest version:

        stack upgrade

3. Fetch the code.

        git clone --recurse-submodules https://github.com/AshleyYakeley/Truth.git
        cd Truth

4. Build. The first time, this will take about an hour or so.

        make

    This will create a Debian package in the `out` directory.

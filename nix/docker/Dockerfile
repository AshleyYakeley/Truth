# Image version should match nix version in input.nixpkgs.follows
FROM nixos/nix:2.28.2

# set up nix
COPY nix.conf /etc/nix/nix.conf

# pre-build GHC
RUN mkdir -p /setup
COPY flake.nix flake.lock stack.yaml stack.yaml.lock /setup/
WORKDIR "/setup"
RUN nix build .#devShell.x86_64-linux.ghc

# set up session and workspace to run in
RUN nix-env -iA nixpkgs.utillinux nixpkgs.gnumake
COPY session /sbin/
VOLUME "/workspace"
WORKDIR "/workspace"
ENTRYPOINT ["/sbin/session"]
CMD ["bash"]

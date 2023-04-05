FROM nixos/nix
RUN echo 2023-05-02 > /container.date
RUN nix-channel --update
RUN nix-env -iA nixpkgs.shadow nixpkgs.stack
RUN mkdir -p /home/user
VOLUME "/workspace"
WORKDIR "/workspace"

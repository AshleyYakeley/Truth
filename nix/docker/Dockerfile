FROM nixos/nix
RUN echo 2023-05-09 > /container.date
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf
RUN echo "accept-flake-config = true" >> /etc/nix/nix.conf
RUN nix-channel --update
RUN nix-env -iA nixpkgs.utillinux nixpkgs.ghc
RUN mkdir -p /home/user
VOLUME "/workspace"
WORKDIR "/workspace"
COPY session /sbin/
ENTRYPOINT ["/sbin/session"]
CMD ["bash"]

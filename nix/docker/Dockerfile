FROM nixos/nix
RUN echo 2023-08-04 > /container.date
COPY nix.conf /etc/nix/nix.conf
RUN nix-channel --update
RUN nix-env -iA nixpkgs.utillinux nixpkgs.ghc
RUN mkdir -p /home/user
VOLUME "/workspace"
WORKDIR "/workspace"
COPY session /sbin/
ENTRYPOINT ["/sbin/session"]
CMD ["bash"]

{
    description = "Pinafore";
    inputs =
    {
        haskellNix.url = "github:AshleyYakeley/haskell.nix?rev=ebdf467c74ce0fc9e43568fd27489ac67d0f9dc8";
        nixpkgs.follows = "haskellNix/nixpkgs-unstable";
        flake-utils.url = "github:numtide/flake-utils";
    };
    outputs = { self, nixpkgs, flake-utils, haskellNix }:
        let
        supportedSystems =
        [
            "x86_64-linux"
            "x86_64-darwin"
            "aarch64-linux"
            "aarch64-darwin"
        ];
        in flake-utils.lib.eachSystem supportedSystems
        (system:
            let
                overlays = [ haskellNix.overlay
                    (final: prev:
                    {
                        pinaforeProject = final.haskell-nix.stackProject'
                        {
                            src = ./.;
                            evalSystem = "x86_64-linux";
                            modules =
                            [
                                {
                                    packages."pinafore-app" =
                                    {
                                        configureFlags = ["-f" "-gitversion"];
                                        dontStrip = false;
                                        dontPatchELF = false;
                                    };
                                }
                            ];

                            # For `nix develop .?submodules=1`
                            shell =
                            {
                                tools =
                                {
                                    cabal = {};
                                    hlint = {};
                                    haskell-language-server = {};
                                };
                                # Non-Haskell shell tools go here
                                buildInputs = with pkgs; [ nixpkgs-fmt ];
                            };
                        };
                    })
                    ];
                pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
                flake = pkgs.pinaforeProject.flake {};
            in flake //
            {
                # Built by `nix build .?submodules=1`
                packages.default = flake.packages."pinafore-app:exe:pinafore";
            }
        );

    # Flake local Nix configuration
    nixConfig =
    {
        # This sets the flake to use the IOG nix cache.
        # Nix should ask for permission before using it, but remove it here if you do not want it to.
        extra-substituters = ["https://cache.iog.io"];
        extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
        allow-import-from-derivation = "true";
    };
}

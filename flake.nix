{
    description = "Pinafore";
    inputs =
    {
        haskellNix.url = "github:AshleyYakeley/haskell.nix?rev=c3881fceb3acbc2ea142b62bdf0f34b52fb49c1b";
        nixpkgs.follows = "haskellNix/nixpkgs-2305";
        flake-utils.url = "github:numtide/flake-utils";
    };
    outputs = { self, nixpkgs, flake-utils, haskellNix }:
        let
            supportedSystems =
            [
                "x86_64-linux"
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
                                    # no X11 server available during testing
                                    packages."changes-gnome" =
                                    {
                                        configureFlags = ["-f" "-trace" "-f" "-test-X11"];
                                    };
                                    packages."pinafore-gnome" =
                                    {
                                        configureFlags = ["-f" "-test-X11"];
                                    };
                                    packages."pinafore-app" =
                                    {
                                        configureFlags = ["-f" "-gitversion"];
                                        dontStrip = false;
                                        dontPatchELF = false;
                                    };
                                }
                            ];
                        };
                    })
                    ];
                pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
                flake = pkgs.pinaforeProject.flake {};
                exePackage = flake.packages."pinafore-app:exe:pinafore";
                stdLibPackage = pkgs.runCommand "pinafore-stdlib" {libdir = ./Pinafore/lib;}
                    ''
                    mkdir -p $out/share/pinafore/lib
                    cp -r $libdir/* $out/share/pinafore/lib/
                    '';
                pinaforePackage = pkgs.symlinkJoin
                {
                    name = "pinafore";
                    paths =
                    [
                        exePackage
                        stdLibPackage
                    ];
                };
            in flake //
            {
                # Built by `nix build .?submodules=1`
                packages.default = pinaforePackage;
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

{
    description = "Pinafore";
    inputs =
    {
        haskellNix.url = "github:input-output-hk/haskell.nix";
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
                        pinaforeProject = final.haskell-nix.stackProject
                        {
                            src =
                            {
                                name = "Pinafore-source";
                                outPath = ./.;
                            };
                            evalSystem = "x86_64-linux";
                            ignorePackageYaml = true;
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
                stdLibPackage = pkgs.runCommand "pinafore-stdlib" {libdir = ./Pinafore/pinafore-stdlib/data;}
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
                packages =
                {
                    default = pinaforePackage;
                    pinafore = pinaforePackage;
                    pinafore-doc = flake.packages."pinafore-app:exe:pinafore-doc";
                };
            }
        );

    # Flake local Nix configuration
    nixConfig =
    {
        # Use IOG nix cache.
        extra-substituters = ["https://cache.iog.io"];
        extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
        allow-import-from-derivation = "true";
    };
}

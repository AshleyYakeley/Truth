{
    description = "Pinafore";
    inputs =
    {
        haskellNix.url = "github:input-output-hk/haskell.nix";
        nixpkgs.follows = "haskellNix/nixpkgs-2311";
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
                pinaforeDocPackage = flake.packages."pinafore-app:exe:pinafore-doc";
                syntaxDataPackage = pkgs.runCommand "pinafore-syntax-data" {}
                    ''
                    ${pinaforeDocPackage}/bin/pinafore-doc --syntax-data > $out
                    '';
                vsceFilePackage = pkgs.runCommand "pinafore-vscode-extension-file" {}
                    ''
                    mkdir -p out/support
                    cp ${syntaxDataPackage} out/support/syntax-data.json
                    cp ${./.}/support/vsc-extension/transform.yq ./
                    cp -r ${./.}/support/vsc-extension/vsce ./
                    chmod -R u+w vsce
                    ${pkgs.yq-go}/bin/yq --from-file transform.yq -o json vsce/package.yaml > vsce/package.json
                    ${pkgs.yq-go}/bin/yq --from-file transform.yq -o json vsce/language-configuration.yaml > vsce/language-configuration.json
                    ${pkgs.yq-go}/bin/yq --from-file transform.yq -o json vsce/syntaxes/pinafore.tmLanguage.yaml > vsce/syntaxes/pinafore.tmLanguage.json
                    PATH=$PATH:${pkgs.nodejs_20}/bin
                    cd vsce && ${pkgs.vsce}/bin/vsce package -o $out
                    '';
                vscePackage = pkgs.runCommand "pinafore-vscode-extension" {}
                    ''
                    mkdir -p $out/share/vscode/extensions/Pinafore.pinafore
                    ${pkgs.unzip}/bin/unzip ${vsceFilePackage}
                    cp -r extension/* $out/share/vscode/extensions/Pinafore.pinafore/
                    '' //
                    {
                        vscodeExtPublisher = "Pinafore";
                        vscodeExtName = "Pinafore";
                        vscodeExtUniqueId = "Pinafore.pinafore";
                        version = "0.4.2";
                    };
            in flake //
            {
                packages =
                {
                    default = pinaforePackage;
                    pinafore = pinaforePackage;
                    pinafore-doc = pinaforeDocPackage;
                    syntax-data = syntaxDataPackage;
                    vscode-extension-file = vsceFilePackage;
                    vscode-extension = vscePackage;
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

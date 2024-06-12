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
            PINAFOREVERSION = "0.6";
            PINAFOREVERSIONABC = PINAFOREVERSION + ".0";
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
                                    packages."pinafore-lib-gnome" =
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
                stdLibPackage = pkgs.runCommand "pinafore-lib-script" {libdir = ./Pinafore/pinafore-lib-script/data;}
                    ''
                    mkdir -p $out/share/pinafore/lib
                    cp -r $libdir/* $out/share/pinafore/lib/
                    '';
                pinadataPackage = flake.packages."pinafore-app:exe:pinadata";
                pinadocPackage = flake.packages."pinafore-docgen:exe:pinadoc";
                pinaforePackage = pkgs.symlinkJoin
                {
                    name = "pinafore";
                    paths =
                    [
                        exePackage
                        pinadocPackage
                        stdLibPackage
                    ];
                };
                syntaxDataPackage = pkgs.runCommand "pinafore-syntax-data" {}
                    ''
                    ${pinadataPackage}/bin/pinadata --syntax-data > $out
                    '';
                VSCXVERSION = "${PINAFOREVERSIONABC}";
                vsceFilePackage = pkgs.runCommand "pinafore-vscode-extension-file" {}
                    ''
                    export VSCXVERSION="${VSCXVERSION}"
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
                        version = "${VSCXVERSION}";
                    };
            in flake //
            {
                packages = flake.packages //
                {
                    default = pinaforePackage;
                    pinafore = pinaforePackage;
                    vscode-extension = vscePackage;
                };
                apps =
                {
                    default = flake.apps."pinafore-app:exe:pinafore";
                    pinafore = flake.apps."pinafore-app:exe:pinafore";
                    pinadoc = flake.apps."pinafore-docgen:exe:pinadoc";
                    pinadata = flake.apps."pinafore-app:exe:pinadata";
                };
                files =
                {
                    syntax-data = syntaxDataPackage;
                    vscode-extension = vsceFilePackage;
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

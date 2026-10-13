{
  description = "Pinafore";
  inputs =
    {
      self.submodules = true;
      nixpkgs =
        {
          follows = "haskellNix/nixpkgs-2605";
        };

      flake-utils =
        {
          type = "github";
          owner = "numtide";
          repo = "flake-utils";
        };

      haskellNix =
        {
          type = "github";
          owner = "input-output-hk";
          repo = "haskell.nix";
          inputs.nixpkgs.follows = "nixpkgs";
        };
    };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      PINAFOREVERSION = "0.6";
      PINAFOREVERSIONABC = PINAFOREVERSION + ".0";
      supportedSystems =
        [
          "x86_64-linux"
        ];
    in
    flake-utils.lib.eachSystem supportedSystems
      (system:
        let
          overlays = [
            haskellNix.overlay
            (final: prev:
              {
                haskell-nix = prev.haskell-nix //
                  {
                    extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings //
                      {
                        "javascriptcoregtk-6.0" = [ "webkitgtk_6_0" ];
                        "webkitgtk-6.0" = [ "webkitgtk_6_0" ];
                        "webkitgtk-web-process-extension-6.0" = [ "webkitgtk_6_0" ];
                      };
                  };
                pinaforeProject = final.haskell-nix.stackProject
                  {
                    src =
                      {
                        name = "Pinafore-source";
                        outPath = ./.;
                      };
                    evalSystem = "x86_64-linux";
                    ignorePackageYaml = true;
                    # Remove once input-output-hk/haskell.nix#2572 is fixed.
                    pkg-def-extras = [{ packages.hgg.revision = null; }];
                    modules =
                      [
                        ({ lib, options, ... }:
                          let
                            packageOptions = options.packages or { };
                            hasPackage = name: builtins.hasAttr name packageOptions;
                            packageSettings =
                              {
                                "unix" =
                                  {
                                    flags."os-string" = true;
                                  };
                                "directory" =
                                  {
                                    flags."os-string" = true;
                                  };
                                "process" =
                                  {
                                    flags."os-string" = true;
                                  };
                                "file-io" =
                                  {
                                    flags."os-string" = true;
                                  };
                                "gi-soup" =
                                  {
                                    components.library.libs = [ pkgs.libsoup ];
                                  };
                                "changes-gnome" =
                                  {
                                    flags."trace" = false;
                                    flags."test-x11" = false;
                                  };
                                "pinafore-lib-gnome" =
                                  {
                                    flags."test-x11" = false;
                                  };
                                "pinafore-app" =
                                  {
                                    flags."gitversion" = false;
                                    dontStrip = false;
                                    dontPatchELF = false;
                                  };
                              };
                          in
                          {
                            packages = lib.filterAttrs (name: _: hasPackage name) packageSettings;
                          })
                      ];
                  };
              })
          ];
          pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
          knownPinaforePackageSet =
            {
              pinafore =
                {
                  src = Pinafore/pinafore-language;
                  package = "pinafore-language";
                  module = "Pinafore.Main";
                  name = "pinaforePackages";
                };
              media =
                {
                  src = Pinafore/pinafore-lib-media;
                  package = "pinafore-lib-media";
                  module = "Pinafore.Library.Media";
                  name = "mediaPackages";
                };
              gnome =
                {
                  src = Pinafore/pinafore-lib-gnome;
                  package = "pinafore-lib-gnome";
                  module = "Pinafore.Library.GNOME";
                  name = "gnomePackages";
                };
              script =
                {
                  src = Pinafore/pinafore-lib-script;
                  package = "pinafore-lib-script";
                  module = "Pinafore.Library.Script";
                  name = "scriptPackages";
                };
            };
          defaultPinaforePackageList = builtins.attrValues knownPinaforePackageSet;
          # Planning uses the assembled project, but unchanged packages compile
          # from the original sources, independent of the selected registrations.
          sharedPackageSources = pkgs.lib.mapAttrs (_: p: {
            src = pkgs.lib.mkForce p.src;
          }) (pkgs.lib.filterAttrs (name: p:
            name != "pinafore-app" && (p.isProject or false)
          ) pkgs.pinaforeProject.hsPkgs);
          mkProject = pinaforePackageList: let
              # Preserve in-tree paths for relative Hpack includes and existing
              # Stack entries. Copy external sources into separate directories.
              projectPrefix = toString ./. + "/";
              sourcePackages = pkgs.lib.imap0
                (i: p:
                  let
                    sourcePath = toString p.src;
                    inProject = pkgs.lib.hasPrefix projectPrefix sourcePath;
                  in {
                    descriptor = p;
                    inherit inProject;
                    directory = if inProject
                      then pkgs.lib.removePrefix projectPrefix sourcePath
                      else "nix-packages/package-${toString i}";
                  })
                pinaforePackageList;
              packageDirsFile = pkgs.writeText "package-dirs.json"
                (builtins.toJSON (map (p: p.directory) sourcePackages));
              configJSON = builtins.toJSON
                {
                  packages = map (p: { inherit (p) package module name; }) pinaforePackageList;
                };
              configJSONFile = pkgs.writeText "config.json" configJSON;
              configuredSource = pkgs.runCommand "Pinafore-configured-source"
                {
                  nativeBuildInputs = [ pkgs.buildPackages.yq-go pkgs.buildPackages.j2cli pkgs.buildPackages.haskellPackages.hpack ];
                }
                ''
                  cp -r ${./.} $out
                  chmod -R u+w $out
                  cd $out
                  ${pkgs.lib.concatMapStringsSep "\n" (p: pkgs.lib.optionalString (!p.inProject) ''
                    mkdir -p ${pkgs.lib.escapeShellArg p.directory}
                    cp -r ${pkgs.lib.escapeShellArg "${p.descriptor.src}/."} ${pkgs.lib.escapeShellArg p.directory}/
                  '') sourcePackages}
                  yq --inplace '.packages = ((.packages + load("${packageDirsFile}")) | unique)' stack.yaml
                  yq --input-format=json --output-format=yaml '[.packages[].package]' \
                    ${configJSONFile} > Pinafore/pinafore-app/generated-dependencies.yaml
                  mkdir -p Pinafore/pinafore-app/lib/Pinafore
                  j2 --format=json Pinafore/pinafore-app/lib/Pinafore/Packages.hs.jinja \
                    ${configJSONFile} > Pinafore/pinafore-app/lib/Pinafore/Packages.hs
                  ${pkgs.lib.concatMapStringsSep "\n" (p: ''
                    if test -f ${pkgs.lib.escapeShellArg "${p.directory}/package.yaml"}; then
                      chmod -R u+w ${pkgs.lib.escapeShellArg p.directory}
                      hpack --force ${pkgs.lib.escapeShellArg p.directory}
                    fi
                  '') sourcePackages}
                  hpack --force Pinafore/pinafore-app
                '';
              project = pkgs.pinaforeProject.appendModule {
                src = pkgs.lib.mkForce configuredSource;
                modules = [{ packages = sharedPackageSources; }];
              };
            in project;
          defaultProject = mkProject defaultPinaforePackageList;
          defaultFlake = defaultProject.flake { };
          mkPackage = pinaforePackageList:
            let
              project = mkProject pinaforePackageList;
              flake = project.flake { };
              exePackage = flake.packages."pinafore-app:exe:pinafore1";
              stdLibPackage = pkgs.runCommand "pinafore-lib-script" { libdir = ./Pinafore/pinafore-lib-script/data; }
                ''
                  mkdir -p $out/share/pinafore/lib
                  cp -r $libdir/* $out/share/pinafore/lib/
                '';
              pinadocPackage = flake.packages."pinafore-docgen:exe:pinadoc";
            in pkgs.symlinkJoin
              {
                name = "pinafore";
                passthru.configuredSource = project.args.src;
                paths =
                  [
                    exePackage
                    pinadocPackage
                    stdLibPackage
                  ];
              };
          # Syntax data does not depend on the selected Pinafore packages.
          baseFlake = pkgs.pinaforeProject.flake { };
          pinadataPackage = baseFlake.packages."pinafore-language:exe:pinadata";
          pinaforePackage = mkPackage defaultPinaforePackageList //
            {
              withPackages = call: mkPackage (call knownPinaforePackageSet);
            };
          syntaxDataFile = pkgs.runCommand "pinafore-syntax-data" { }
            ''
              ${pinadataPackage}/bin/pinadata --syntax-data > $out
            '';

          # Support
          support = { inherit pkgs PINAFOREVERSIONABC syntaxDataFile; branding = support/branding; };
          vsce = import support/vsc-extension support;

          # App and stuff
          app = defaultFlake.apps."pinafore-app:exe:pinafore1".program;
          minscript = pkgs.writeText "minscript" "pure ()";
          importscript = pkgs.writeText "importscript" "import \"UILib\" pure ()";
          interpretFileCheck = pkgs.runCommand "interpretFileCheck" { } "${app} -n ${minscript} > $out";
          runFileCheck = pkgs.runCommand "runFileCheck" { } "${app} ${minscript} > $out";
          importFileCheck = pkgs.runCommand "importFileCheck" { } "${app} ${importscript} > $out";
        in
        {
          packages =
          {
            default = pinaforePackage;
            pinafore = pinaforePackage;
            vscode-extension = vsce.package;
          };
          pinaforePackages = knownPinaforePackageSet;
          apps =
            {
              default = defaultFlake.apps."pinafore-app:exe:pinafore1";
              pinafore = defaultFlake.apps."pinafore-app:exe:pinafore1";
              pinafore1 = defaultFlake.apps."pinafore-app:exe:pinafore1";
              pinadoc = defaultFlake.apps."pinafore-docgen:exe:pinadoc";
              pinadata = baseFlake.apps."pinafore-language:exe:pinadata";
            };
          checks = defaultFlake.checks //
          {
            interpretFile = interpretFileCheck;
            runFile = runFileCheck;
            importFile = importFileCheck;
          };
          files =
            {
              syntax-data = syntaxDataFile;
              vscode-extension = vsce.file;
            };
          formatter = pkgs.nixpkgs-fmt;
          devShells =
            let
              shellInputs =
                {
                  tools =
                    {
                      haskell-language-server = "2.12.0.0";
                    };
                  buildInputs = with pkgs;
                    [
                      bashInteractive
                      gnumake
                      docker
                      xhost
                      stack
                      j2cli
                      yq-go
                      haskellPackages.hlint
                      haskellPackages.implicit-hie
                    ];
                  shellHook = ''
                    export PATH="$PWD/bin:$PATH"
                  '';
                };
            in
            {
              default = defaultProject.shellFor shellInputs;
              hoogle = defaultProject.shellFor (shellInputs //
              {
                withHoogle = true;
                additional = ps: with ps;
                  [
                    thread-trace
                    shapes
                    cairo-functional
                    changes-core
                    changes-media
                    changes-world
                    changes-gnome

                    polar-shim
                    typed-expression
                    pinafore-base
                    pinafore-storage
                    pinafore-syntax
                    pinafore-language
                    pinafore-lib-media
                  ];
              });
            };
        }
      );

  # Flake local Nix configuration
  nixConfig =
    {
      # Use IOG nix cache.
      extra-substituters = [ "https://cache.iog.io" ];
      extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
      allow-import-from-derivation = "true";
    };
}

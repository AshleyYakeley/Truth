{ pkgs
, PINAFOREVERSIONABC
, syntaxDataFile
, branding
, ...
}:
let
  VSCXVERSION = "${PINAFOREVERSIONABC}";
  file = pkgs.runCommand "pinafore-vscode-extension-file" { }
    ''
      export VSCXVERSION="${VSCXVERSION}"
      mkdir -p out/support
      cp ${syntaxDataFile} out/support/syntax-data.json
      cp ${./.}/transform.yq ./
      cp -r ${./.}/vsce ./
      chmod -R u+w vsce
      ${pkgs.yq-go}/bin/yq --from-file transform.yq -o json vsce/package.yaml > vsce/package.json
      ${pkgs.yq-go}/bin/yq --from-file transform.yq -o json vsce/language-configuration.yaml > vsce/language-configuration.json
      ${pkgs.yq-go}/bin/yq --from-file transform.yq -o json vsce/syntaxes/pinafore.tmLanguage.yaml > vsce/syntaxes/pinafore.tmLanguage.json
      mkdir -p vsce/images
      ${pkgs.librsvg}/bin/rsvg-convert -w 256 -h 256 ${branding}/logo.svg -o vsce/images/logo.png
      PATH=$PATH:${pkgs.nodejs}/bin
      cd vsce && ${pkgs.vsce}/bin/vsce package -o $out
    '';
  package = pkgs.runCommand "pinafore-vscode-extension" { }
    ''
      mkdir -p $out/share/vscode/extensions/Pinafore.pinafore
      ${pkgs.unzip}/bin/unzip ${file}
      cp -r extension/* $out/share/vscode/extensions/Pinafore.pinafore/
    '' //
  {
    vscodeExtPublisher = "Pinafore";
    vscodeExtName = "Pinafore";
    vscodeExtUniqueId = "Pinafore.pinafore";
    version = "${VSCXVERSION}";
  };
in
{ inherit file package; }

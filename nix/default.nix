{ paths ? import ./paths.nix {}
, pkgs ? import paths.nixpkgs {}
, haskellPackages ? pkgs.haskellPackages
}:
let
  inherit (pkgs) makeWrapper glib_networking;
  inherit (pkgs.gnome3) gsettings_desktop_schemas;
  inherit (haskellPackages) webkit cabal gtk cabalInstall;
  httpsEverywhereRules = import paths.httpsEverywhereRules { inherit pkgs; };
in cabal.mkDerivation (self: {
  pname = "hwb";
  version = "0.0.1";
  src = with builtins; filterSource (path: _: !(elem (baseNameOf path) [".git" "dist"])) ../.;
  isLibrary = false;
  isExecutable = true;
  doCheck = false;
  buildDepends = [ gtk webkit httpsEverywhereRules ];
  buildTools = [ cabalInstall makeWrapper ];
  postInstall = ''
    wrapProgram $out/bin/hwb \
      --prefix GIO_EXTRA_MODULES : $(find ${glib_networking} -type d -name modules) \
      --prefix XDG_DATA_DIRS : $(find ${gsettings_desktop_schemas}/share/gsettings-schemas/ -mindepth 1 -maxdepth 1 -type d)
  '';
})

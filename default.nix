let nixpkgs = (import <nixpkgs> {}).fetchgit {
  url = "git://github.com/nixos/nixpkgs.git";
  rev = "ef5c48326a7fd7f6d2dc944634afc6b55381dd6d";
  sha256 = "17kbhjgrpza61a2y8v1h21p8cmrdpvdajd0zrcb8vh18hw5pqa3i";
}; in
{ pkgs ? (import nixpkgs {})
, haskellPackages ? pkgs.haskellPackages
, src ? (with builtins; filterSource (path: _: !(elem (baseNameOf path) [".git" "dist"])) ./.)
}:
let
  inherit (pkgs) makeWrapper glib_networking;
  inherit (pkgs.gnome3) gsettings_desktop_schemas;
  inherit (haskellPackages) webkit cabal gtk cabalInstall;
in cabal.mkDerivation (self: {
  pname = "hwb";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  doCheck = false;
  buildDepends = [ gtk webkit ];
  buildTools = [ cabalInstall makeWrapper ];
  postInstall = ''
    wrapProgram $out/bin/hwb \
      --prefix GIO_EXTRA_MODULES : $(find ${glib_networking} -type d -name modules) \
      --prefix XDG_DATA_DIRS : $(find ${gsettings_desktop_schemas}/share/gsettings-schemas/ -mindepth 1 -maxdepth 1 -type d)
  '';
})

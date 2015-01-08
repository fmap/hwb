{ fetchgit ? (import <nixpkgs> {}).fetchgit }:
{ nixpkgs = fetchgit {
    url    = "git://github.com/nixos/nixpkgs.git";
    rev    = "ef5c48326a7fd7f6d2dc944634afc6b55381dd6d";
    sha256 = "17kbhjgrpza61a2y8v1h21p8cmrdpvdajd0zrcb8vh18hw5pqa3i";
  };
  httpsEverywhereRules = fetchgit {
    url    = "git://github.com/fmap/https-everywhere-rules.git";
    rev    = "c50f8496d3148b64855b61adfedbc48174e4e263";
    sha256 = "e6f80217d7db5fbeccfd227d89b69e8b6dbda5334837b62831af1152219e1042";
  };
}

{ fetchgit ? (import <nixpkgs> {}).fetchgit }:
{ nixpkgs = fetchgit {
    url    = "git://github.com/nixos/nixpkgs.git";
    rev    = "ef5c48326a7fd7f6d2dc944634afc6b55381dd6d";
    sha256 = "17kbhjgrpza61a2y8v1h21p8cmrdpvdajd0zrcb8vh18hw5pqa3i";
  };
  httpsEverywhereRules = fetchgit {
    url    = "git://github.com/fmap/https-everywhere-rules.git";
    rev    = "16959f6ca07d4d982777b90eecede7d51c1d6fb5";
    sha256 = "0d181196a7aa94c3bfa5bdae6dd85c18b01fd79fb618a1db16b069adb3d92d44";
  };
}

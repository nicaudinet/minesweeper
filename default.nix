with (import (builtins.fetchGit {
  url = "https://github.com/dmjio/miso";
  ref = "refs/tags/1.8.2";
}) {});

pkgs.haskellPackages.callCabal2nix "minesweeper" ./. {}

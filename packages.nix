{}:
  let
    nixpkgs = import (builtins.fetchTarball {
      name = "nixpkgs-unstable-2020-01-26";
      url = https://github.com/NixOS/nixpkgs-channels/archive/0c960262d159d3a884dadc3d4e4b131557dad116.tar.gz;
      sha256 = "sha256:0d7ms4dxbxvd6f8zrgymr6njvka54fppph1mrjjlcan7y0dhi5rb";
    }) {};
  in nixpkgs

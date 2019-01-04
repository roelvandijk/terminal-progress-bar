# To calculate sha256:
# > nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/3fd87ad0073fd1ef71a8fcd1a1d1a89392c33d0a.tar.gz
builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/3fd87ad0073fd1ef71a8fcd1a1d1a89392c33d0a.tar.gz";
  sha256 = "0n4ffwwfdybphx1iyqz1p7npk8w4n78f8jr5nq8ldnx2amrkfwhl";
}

# To calculate sha256:
# > nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/3fd87ad0073fd1ef71a8fcd1a1d1a89392c33d0a.tar.gz
builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/50d5d73e22bb2830f490e26a528579facfc7f302.tar.gz";
  sha256 = "0c1inf0pc2jizkrfl3629s154r55ya5asmwnwn6g64ppz2wwzizi";
}

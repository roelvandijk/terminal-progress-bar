# To calculate sha256:
# > nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/07723f14bb132f6d65c74b953bef39cd80457273.tar.gz
builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/07723f14bb132f6d65c74b953bef39cd80457273.tar.gz";
  sha256 = "1aw5fs47hby67klg8h4awaqajcgpr60xhwyc84s51my8vr4xrs84";
}

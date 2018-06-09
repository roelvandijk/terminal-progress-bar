final : previous : with final.haskell.lib; {
  haskell = previous.haskell // {
    packageOverrides = self : super : {
      base-compat-batteries = doJailbreak super.base-compat-batteries;
      base-compat = super.base-compat_0_10_1;
      criterion = super.criterion_1_4_1_0;

      terminal-progress-bar =
        let src = previous.runCommand "terminal-progress-bar-src" {
                    lib     = ./lib;
                    LICENSE = ./LICENSE;
                  } ''
                  mkdir -p $out
                  cp -r $lib/src   $out/src
                  cp -r $lib/test  $out/test
                  cp -r $lib/bench $out/bench
                  cp $LICENSE $out/LICENSE
                  cp $lib/terminal-progress-bar.cabal $out
                  '';
        in doBenchmark (super.callCabal2nix "terminal-progress-bar" src {});

      terminal-progress-bar-example =
        let src = previous.runCommand "terminal-progress-bar-example-src" {
                    example = ./example;
                    LICENSE = ./LICENSE;
                  } ''
                  mkdir -p $out
                  cp $example/example.hs $out
                  cp $LICENSE $out/LICENSE
                  cp $example/terminal-progress-bar-example.cabal $out
                  '';
        in super.callCabal2nix "terminal-progress-bar-example" src {};
    };
  };
}

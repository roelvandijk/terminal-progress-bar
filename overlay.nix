final : previous : with final.haskell.lib; {
  haskell = previous.haskell // {
    packageOverrides = self : super : {
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

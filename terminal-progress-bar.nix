{ mkDerivation, ansi-terminal, async, base, criterion, HUnit, stdenv, stm, random
, stm-chans, terminal-size, test-framework, test-framework-hunit, text
}:
mkDerivation {
  pname = "terminal-progress-bar";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    async base stm stm-chans terminal-size text
  ];
  executableHaskellDepends = [
    ansi-terminal async base random text
  ];
  testHaskellDepends = [
    base criterion HUnit test-framework test-framework-hunit text
  ];
  homepage = "https://github.com/roelvandijk/terminal-progress-bar";
  description = "A simple progress bar in the terminal";
  license = stdenv.lib.licenses.bsd3;
}

{-# LANGUAGE NoImplicitPrelude, PackageImports, UnicodeSyntax #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Control.Monad      ( (=<<) )
import "base" Data.Function      ( ($) )
import "base" Prelude            ( String )
import "base" System.Environment ( getArgs )
import "base" System.IO          ( IO )
import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import "HUnit" Test.HUnit.Base ( assertEqual )
import "test-framework" Test.Framework ( defaultMainWithOpts
                                       , interpretArgsOrExit
                                       , Test, testGroup
                                       )
import "test-framework-hunit" Test.Framework.Providers.HUnit ( testCase )
import "this" System.ProgressBar ( mkProgressBar
                                 , Label, noLabel, msg, percentage, exact
                                 )

--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main ∷ IO ()
main = do opts ← interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests opts

tests ∷ [Test]
tests =
  [ testGroup "Label padding"
    [ eqTest "no labels"  "[]"          noLabel     noLabel      0 0 0
    , eqTest "pre"        "pre []"      (msg "pre") noLabel      0 0 0
    , eqTest "post"       "[] post"     noLabel     (msg "post") 0 0 0
    , eqTest "pre & post" "pre [] post" (msg "pre") (msg "post") 0 0 0
    ]
  , testGroup "Bar fill"
    [ eqTest "empty"    "[....]" noLabel noLabel 6 0 1
    , eqTest "full"     "[====]" noLabel noLabel 6 1 1
    , eqTest "half"     "[==..]" noLabel noLabel 6 1 2
    , eqTest "overfull" "[====]" noLabel noLabel 6 2 1
    ]
  , testGroup "Percentage label"
    [ eqTest "  0%" "  0% [....]" percentage noLabel 11 0 1
    , eqTest "100%" "100% [====]" percentage noLabel 11 1 1
    , eqTest " 50%" " 50% [==..]" percentage noLabel 11 1 2
    , eqTest "200%" "200% [====]" percentage noLabel 11 2 1
    ]
  , testGroup "Exact label"
    [ eqTest "0/0" "0/0 [....]" exact noLabel 10 0 0
    , eqTest "1/1" "1/1 [====]" exact noLabel 10 1 1
    , eqTest "1/2" "1/2 [==..]" exact noLabel 10 1 2
    , eqTest "2/1" "2/1 [====]" exact noLabel 10 2 1
    ]
  ]

eqTest ∷ String → String → Label → Label → ℤ → ℤ → ℤ → Test
eqTest name expected mkPreLabel mkPostLabel width todo done =
    testCase name $ assertEqual errMsg expected actual
  where
    actual = mkProgressBar mkPreLabel mkPostLabel width todo done
    errMsg = "Expected result doesn't match actual result"


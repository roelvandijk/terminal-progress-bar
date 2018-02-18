{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" System.Environment ( getArgs )
import "HUnit" Test.HUnit.Base ( assertEqual )
import "test-framework" Test.Framework
    ( defaultMainWithOpts, interpretArgsOrExit, Test, testGroup )
import "test-framework-hunit" Test.Framework.Providers.HUnit ( testCase )
import "terminal-progress-bar" System.ProgressBar
import qualified "text" Data.Text.Lazy as TL

--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main :: IO ()
main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests opts

tests :: [Test]
tests =
  [ testGroup "Label padding"
    [ eqTest "no labels"  "[]"          noLabel     noLabel      0 $ Progress 0 0
    , eqTest "pre"        "pre []"      (msg "pre") noLabel      0 $ Progress 0 0
    , eqTest "post"       "[] post"     noLabel     (msg "post") 0 $ Progress 0 0
    , eqTest "pre & post" "pre [] post" (msg "pre") (msg "post") 0 $ Progress 0 0
    ]
  , testGroup "Bar fill"
    [ eqTest "empty"       "[....]" noLabel noLabel 6 $ Progress  0   1
    , eqTest "almost half" "[=>..]" noLabel noLabel 6 $ Progress 49 100
    , eqTest "half"        "[==>.]" noLabel noLabel 6 $ Progress  1   2
    , eqTest "almost full" "[===>]" noLabel noLabel 6 $ Progress 99 100
    , eqTest "full"        "[====]" noLabel noLabel 6 $ Progress  1   1
    , eqTest "overfull"    "[====]" noLabel noLabel 6 $ Progress  2   1
    ]
  , testGroup "Labels"
    [ testGroup "Percentage"
      [ eqTest "  0%" "  0% [....]" percentage noLabel 11 $ Progress 0 1
      , eqTest "100%" "100% [====]" percentage noLabel 11 $ Progress 1 1
      , eqTest " 50%" " 50% [==>.]" percentage noLabel 11 $ Progress 1 2
      , eqTest "200%" "200% [====]" percentage noLabel 11 $ Progress 2 1
      , labelTest "0 work todo" percentage (Progress 10 0) "100%"
      ]
    , testGroup "Exact"
      [ eqTest "0/0" "0/0 [....]" exact noLabel 10 $ Progress 0 0
      , eqTest "1/1" "1/1 [====]" exact noLabel 10 $ Progress 1 1
      , eqTest "1/2" "1/2 [==>.]" exact noLabel 10 $ Progress 1 2
      , eqTest "2/1" "2/1 [====]" exact noLabel 10 $ Progress 2 1
      , labelTest "0 work todo" exact (Progress 10 0) "10/0"
      ]
    ]
  ]

labelTest :: String -> Label -> Progress -> TL.Text -> Test
labelTest testName label progress expected =
    testCase testName $ assertEqual expectationError expected (label progress)

eqTest :: String -> TL.Text -> Label -> Label -> Int -> Progress -> Test
eqTest name expected mkPreLabel mkPostLabel width progress =
    testCase name $ assertEqual expectationError expected actual
  where
    actual = mkProgressBar opts progress

    opts :: ProgressOptions Progress
    opts = defProgressOptions
           { progressOptPrefix = mkPreLabel
           , progressOptPostfix = mkPostLabel
           , progressOptWidth = ConstantWidth width
           }

expectationError :: String
expectationError = "Expected result doesn't match actual result"

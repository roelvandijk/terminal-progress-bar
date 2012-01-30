{-# LANGUAGE NoImplicitPrelude, PackageImports, UnicodeSyntax #-}

module System.ProgressBar
    ( -- * Progress bars
      progressBar
    , mkProgressBar
      -- * Labels
    , Label
    , noLabel
    , percentage
    , exact
    ) where

import "base" Control.Monad ( when )
import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($) )
import "base" Data.Int      ( Int )
import "base" Data.List     ( (++), null, genericLength, genericReplicate )
import "base" Data.Ord      ( min, max )
import "base" Data.Ratio    ( (%) )
import "base" Data.String   ( String )
import "base" Prelude       ( (-), round )
import "base" System.IO     ( IO, putStr, putChar )
import "base" Text.Printf   ( printf )
import "base" Text.Show     ( show )
import "base-unicode-symbols" Data.Eq.Unicode ( (≢) )
import "base-unicode-symbols" Prelude.Unicode ( ℤ, ℚ, (⋅) )


-- | Print a progress bar
--
-- Erases the current line! (by outputting '\r') Does not print a
-- newline '\n'. Subsequent invocations will overwrite the previous
-- output.
progressBar ∷ Label -- ^ Prefixed label.
            → Label -- ^ Postfixed label.
            → ℤ     -- ^ Total progress bar width in characters.
            → ℤ     -- ^ Amount of work completed.
            → ℤ     -- ^ Total amount of work.
            → IO ()
progressBar mkPreLabel mkPostLabel width todo done = do
    putChar '\r'
    putStr $ mkProgressBar mkPreLabel mkPostLabel width todo done

mkProgressBar ∷ Label
              → Label
              → ℤ
              → ℤ
              → ℤ
              → String
mkProgressBar mkPreLabel mkPostLabel width todo done =
    printf "%s%s[%s%s]%s%s"
           preLabel
           (pad preLabel)
           (genericReplicate completed '=')
           (genericReplicate remaining '.')
           (pad postLabel)
           postLabel
  where
    fraction = todo % done

    effectiveWidth = max 0 $ width - 2 - genericLength preLabel - genericLength postLabel

    completed = min effectiveWidth $ round $ fraction ⋅ (effectiveWidth % 1)
    remaining = effectiveWidth - completed

    preLabel  = mkPreLabel  todo done
    postLabel = mkPostLabel todo done

    pad ∷ String → String
    pad s | null s    = ""
          | otherwise = " "


type Label = ℤ → ℤ → String

noLabel ∷ Label
noLabel _ _ = ""

percentage ∷ Label
percentage done todo = printf "%3i%%" (round (done % todo ⋅ 100) ∷ ℤ)

exact ∷ Label
exact done todo = show done ++ "/" ++ show todo

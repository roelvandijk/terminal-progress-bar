{-# language PackageImports #-}

module System.ProgressBar
    ( -- * Progress bars
      ProgressBar
    , progressBar
    , autoProgressBar
    , hProgressBar
    , mkProgressBar
      -- * Progress state
    , Progress(..)
      -- * Labels
    , Label
    , noLabel
    , msg
    , percentage
    , exact
      -- * Auto printing
    , ProgressRef
    , startProgress
    , incProgress
    ) where

import "base" System.IO ( Handle, stderr )
import "base" Control.Concurrent ( ThreadId )
import           "this" System.ProgressBar.State ( Progress(..) )
import qualified "this" System.ProgressBar.State as State

-- | Type of functions producing a progress bar.
type ProgressBar a
   = Label -- ^ Prefixed label.
  -> Label -- ^ Postfixed label.
  -> Integer
     -- ^ Total progress bar width in characters. Either used as given
     -- or as a default when the width of the terminal can not be
     -- determined.
     --
     -- See 'autoProgressBar'.
  -> Progress -- ^ Current progress.
  -> a

-- | Print a progress bar to 'stderr'
--
-- See 'hProgressBar'.
progressBar :: ProgressBar (IO ())
progressBar = hProgressBar stderr

-- | Print a progress bar to 'stderr' which takes up all available space.
--
-- The given width will be used if the width of the terminal can not
-- be determined.
--
-- See 'hProgressBar'.
autoProgressBar :: ProgressBar (IO ())
autoProgressBar = State.autoProgressBar

-- | Print a progress bar to a file handle.
--
-- Erases the current line! (by outputting '\r') Does not print a
-- newline '\n'. Subsequent invocations will overwrite the previous
-- output.
hProgressBar :: Handle -> ProgressBar (IO ())
hProgressBar = State.hProgressBar

-- | Renders a progress bar
--
-- >>> mkProgressBar (msg "Working") percentage 40 30 100
-- "Working [=======>.................]  30%"
mkProgressBar :: ProgressBar String
mkProgressBar = State.mkProgressBar

-- | A label that can be pre- or postfixed to a progress bar.
type Label
   = Progress -- ^ Current progress.
  -> String -- ^ Resulting label.

-- | The empty label.
--
-- >>> noLabel 30 100
-- ""
noLabel :: Label
noLabel = State.noLabel

-- | A label consisting of a static string.
--
-- >>> msg "foo" 30 100
-- "foo"
msg :: String -> Label
msg = State.msg

-- | A label which displays the progress as a percentage.
--
-- Constant width property:
-- &#x2200; d t : &#x2115;. d &#x2264; t &#x2192; length (percentage d t) &#x2261; 4
--
-- >>> percentage 30 100
-- " 30%"
--
-- __Note__: if no work is to be done (todo == 0) the percentage will
-- always be 100%.

-- ∀ d t : ℕ. d ≤ t -> length (percentage d t) ≡ 3
percentage :: Label
percentage = State.percentage

-- | A label which displays the progress as a fraction of the total
-- amount of work.
--
-- Equal width property:
-- &#x2200; d&#x2081; d&#x2082; t : &#x2115;. d&#x2081; &#x2264; d&#x2082; &#x2264; t &#x2192; length (exact d&#x2081; t) &#x2261; length (exact d&#x2082; t)
--
-- >>> exact 30 100
-- " 30/100"

-- ∀ d₁ d₂ t : ℕ. d₁ ≤ d₂ ≤ t -> length (exact d₁ t) ≡ length (exact d₂ t)
exact :: Label
exact = State.exact

-- * Auto-Printing Progress

type ProgressRef = State.ProgressRef Progress

-- | Start a thread to automatically display progress. Use incProgress to step
-- the progress bar.
startProgress
    :: Label -- ^ Prefixed label.
    -> Label -- ^ Postfixed label.
    -> Integer
       -- ^ Total progress bar width in characters. Only used if the
       -- width can not be automatically determined.
    -> Progress -- ^ Initial progress state.
    -> IO (ProgressRef, ThreadId)
startProgress = State.startProgress

-- | Increment the progress bar. Negative values will reverse the progress.
-- Progress will never be negative and will silently stop taking data
-- when it completes.
incProgress :: ProgressRef -> Integer -> IO ()
incProgress pr amount =
    State.incProgress pr
      (\st -> st { progressDone = progressDone st + amount })

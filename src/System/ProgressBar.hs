{-# language PackageImports #-}

{- |
A progress bar is used to convey the progress of a task. This module
implements a very simple textual progress bar.

The animated progress bar depends entirely on the interpretation of
the carriage return character (\'\\r\'). If your terminal interprets
it as something else than \"move cursor to beginning of line\", the
animation won't work.

See "System.ProgressBar.State" for a more generic interface.
-}
module System.ProgressBar
    ( -- * Progress bars
      progressBar
    , hProgressBar
    , mkProgressBar
      -- * Options
    , ProgressOptions(..)
    , EscapeCode
    , defProgressOptions
    , ProgressBarWidth(..)
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
    , updateProgress
    ) where

import "async" Control.Concurrent.Async ( Async )
import "base" System.IO ( Handle, stderr )
import qualified "text" Data.Text.Lazy as TL
import "this" System.ProgressBar.State
    ( ProgressOptions(..)
    , ProgressBarWidth(..)
    , Progress(..)
    )
import qualified "this" System.ProgressBar.State as State

-- | A default set of progress options.
--
-- You can override some fields of the default instead of specifying
-- all the fields of a 'ProgressOptions' record.
--
-- The default does not use any escape sequences.
defProgressOptions :: ProgressOptions Progress
defProgressOptions = State.defProgressOptions

-- | Print a progress bar to 'stderr'
--
-- See 'hProgressBar'.
progressBar :: ProgressOptions Progress -> Progress -> IO ()
progressBar = hProgressBar stderr

-- | Print a progress bar to a file handle.
--
-- Erases the current line! (by outputting '\r') Does not print a
-- newline '\n'. Subsequent invocations will overwrite the previous
-- output.
hProgressBar :: Handle -> ProgressOptions Progress -> Progress -> IO ()
hProgressBar = State.hProgressBar

-- | Renders a progress bar
--
-- @
-- let opts =
--       'defProgressOptions'
--       { progressOptPrefix  = msg \"Working"
--       , progressOptPostfix = percentage
--       , progressOptWidth   = ConstantWidth 40
--       }
-- @
--
-- >>> mkProgressBar opts (Progress 30 100)
-- "Working [=======>.................]  30%"
--
-- Not that this function treats 'TerminalWidth' the same as
-- 'ConstantWidth' because it doesn't use 'IO'. Use 'progressBar' or
-- 'hProgressBar' to get automatic width.
mkProgressBar :: ProgressOptions Progress -> Progress -> TL.Text
mkProgressBar = State.mkProgressBar

-- | An escape code is a sequence of bytes which the terminal looks
-- for and interprets as commands, not as character codes.
--
-- It is vital that the output of this function, when send to the
-- terminal, does not result in characters being drawn.
type EscapeCode
   = Progress -- ^ Current progress.
  -> TL.Text -- ^ Resulting escape code. Must be non-printable.

-- | A label that can be pre- or postfixed to a progress bar.
type Label
   = Progress -- ^ Current progress.
  -> TL.Text -- ^ Resulting label.

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
msg :: TL.Text -> Label
msg = State.msg

-- | A label which displays the progress as a percentage.
--
-- Constant width property:
-- \(\forall d t : \mathbb{N}. d \leq t \rightarrow \texttt{(length \$ percentage \$ Progress d t)} \equiv 4\)
--
-- >>> percentage 30 100
-- " 30%"
--
-- __Note__: if no work is to be done (todo == 0) the percentage will
-- always be 100%.
percentage :: Label
percentage = State.percentage

-- | A label which displays the progress as a fraction of the total
-- amount of work.
--
-- Equal width property - the length of the resulting label is a function of the
-- total amount of work:
--
-- \(\forall d_1 d_2 t : \mathbb{N}. d_1 \leq d_2 \leq t \rightarrow \texttt{(length \$ exact \$ Progress d1 t)} \equiv \texttt{(length \$ exact \$ Progress d2 t)}\)
--
-- >>> exact 30 100
-- " 30/100"
exact :: Label
exact = State.exact

--------------------------------------------------------------------------------

-- | A reference to the state of some progress bar started with 'startProgress'.
--
-- This reference can be passed to 'updateProgress'.
type ProgressRef = State.ProgressRef Progress

-- | Start a thread to automatically display progress.
--
-- Use 'updateProgress' to update the progress bar.
startProgress
    :: ProgressOptions Progress
    -> Progress -- ^ Initial progress state.
    -> IO (ProgressRef, Async ())
startProgress = State.startProgress

-- | Updates the state of a progress bar started with 'startProgress'.
--
-- Progress will never be negative and will silently stop taking data
-- when it completes.
updateProgress
    :: ProgressRef -- ^ Reference obtained from 'startProgress'.
    -> Int -- ^ Amount by which to change the current progress.
    -> IO ()
updateProgress pr amount =
    State.updateProgress pr
      (\st -> st { progressDone = progressDone st + amount })

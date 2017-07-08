{-# language PackageImports #-}
{-# OPTIONS_HADDOCK not-home #-}

module System.ProgressBar.State
    ( -- * Progress bars
      ProgressBar
    , progressBar
    , autoProgressBar
    , hProgressBar
    , mkProgressBar
      -- * Labels
    , LabelAmount(..)
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

import "base" Control.Monad ( when )
import "base" Data.List     ( genericLength, genericReplicate )
import "base" Data.Ratio    ( (%) )
import "base" System.IO     ( Handle, stderr, hPutChar, hPutStr, hFlush )
import "base" Text.Printf   ( printf )
import "base" Control.Concurrent ( ThreadId, forkIO )
import "stm"  Control.Concurrent.STM
    ( TVar, readTVar, writeTVar, newTVar, atomically, STM )
import "stm-chans"  Control.Concurrent.STM.TMQueue
    ( TMQueue, readTMQueue, closeTMQueue, writeTMQueue, newTMQueue )
import qualified "terminal-size" System.Console.Terminal.Size as TS

-- | Type of functions producing a progress bar.
type ProgressBar s a
   = Label s -- ^ Prefixed label.
  -> Label s -- ^ Postfixed label.
  -> Integer
     -- ^ Total progress bar width in characters. Either used as given
     -- or as a default when the width of the terminal can not be
     -- determined.
     --
     -- See 'autoProgressBar'.
  -> s -- ^ Progress bar state.
  -> a

-- | Print a progress bar to 'stderr'
--
-- See 'hProgressBar'.
progressBar :: (LabelAmount s) => ProgressBar s (IO ())
progressBar = hProgressBar stderr

-- | Print a progress bar to 'stderr' which takes up all available space.
--
-- The given width will be used if the width of the terminal can not
-- be determined.
--
-- See 'hProgressBar'.
autoProgressBar :: (LabelAmount s) => ProgressBar s (IO ())
autoProgressBar mkPreLabel mkPostLabel defaultWidth st = do
    mbWindow <- TS.size
    let width = maybe defaultWidth TS.width mbWindow
    progressBar mkPreLabel mkPostLabel width st

-- | Print a progress bar to a file handle.
--
-- Erases the current line! (by outputting '\r') Does not print a
-- newline '\n'. Subsequent invocations will overwrite the previous
-- output.
hProgressBar :: LabelAmount s => Handle -> ProgressBar s (IO ())
hProgressBar hndl mkPreLabel mkPostLabel width st = do
    hPutChar hndl '\r'
    hPutStr hndl $ mkProgressBar mkPreLabel mkPostLabel width st
    hFlush hndl

-- | Renders a progress bar
--
-- >>> mkProgressBar (msg "Working") percentage 40 30 100
-- "Working [=======>.................]  30%"
mkProgressBar :: (LabelAmount s) => ProgressBar s String
mkProgressBar mkPreLabel mkPostLabel width st =
    printf "%s%s[%s%s%s]%s%s"
           preLabel
           prePad
           (genericReplicate completed '=')
           (if remaining /= 0 && completed /= 0 then ">" else "")
           (genericReplicate (remaining - if completed /= 0 then 1 else 0)
                             '.'
           )
           postPad
           postLabel
  where
    todo = todoAmount st
    done = doneAmount st

    -- Amount of work completed.
    fraction :: Rational
    fraction | todo /= 0  = done % todo
             | otherwise = 0 % 1

    -- Amount of characters available to visualize the progress.
    effectiveWidth = max 0 $ width - usedSpace
    usedSpace = 2 + genericLength preLabel
                  + genericLength postLabel
                  + genericLength prePad
                  + genericLength postPad

    -- Number of characters needed to represent the amount of work
    -- that is completed. Note that this can not always be represented
    -- by an integer.
    numCompletedChars :: Rational
    numCompletedChars = fraction * (effectiveWidth % 1)

    completed, remaining :: Integer
    completed = min effectiveWidth $ floor numCompletedChars
    remaining = effectiveWidth - completed

    preLabel, postLabel :: String
    preLabel  = mkPreLabel  st
    postLabel = mkPostLabel st

    prePad, postPad :: String
    prePad  = pad preLabel
    postPad = pad postLabel

    pad :: String -> String
    pad s | null s    = ""
          | otherwise = " "

-- | Class for the default progress bar labels.
--
-- Any progress bar state that implements this class can be used by
-- the default label functions.
class LabelAmount s where
    doneAmount :: s -> Integer -- ^ Amount of work completed.
    todoAmount :: s -> Integer -- ^ Total amount of work.

-- | A label that can be pre- or postfixed to a progress bar.
type Label s
   = s      -- ^ Current progress bar state.
  -> String -- ^ Resulting label.

-- | The empty label.
--
-- >>> noLabel st
-- ""
noLabel :: Label s
noLabel = msg ""

-- | A label consisting of a static string.
--
-- >>> msg "foo" st
-- "foo"
msg :: String -> Label s
msg s _ = s

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
percentage :: LabelAmount s => Label s
percentage s
    | todo == 0 = "100%"
    | otherwise = printf "%3i%%" (round (done % todo * 100) :: Integer)
  where
    done = doneAmount s
    todo = todoAmount s

-- | A label which displays the progress as a fraction of the total
-- amount of work.
--
-- Equal width property:
-- &#x2200; d&#x2081; d&#x2082; t : &#x2115;. d&#x2081; &#x2264; d&#x2082; &#x2264; t &#x2192; length (exact d&#x2081; t) &#x2261; length (exact d&#x2082; t)
--
-- >>> exact (30, 100)
-- " 30/100"

-- ∀ d₁ d₂ t : ℕ. d₁ ≤ d₂ ≤ t -> length (exact d₁ t) ≡ length (exact d₂ t)
exact :: LabelAmount s => Label s
exact s = printf "%*i/%s" (length totalStr) (doneAmount s) totalStr
  where
    totalStr = show $ todoAmount s

-- * Auto-Printing Progress

data ProgressRef s
   = ProgressRef
     { prPrefix    :: Label s
     , prPostfix   :: Label s
     , prWidth     :: Integer
     , prState     :: TVar s
     , prQueue     :: TMQueue (s -> s)
     }

-- | Start a thread to automatically display progress. Use incProgress to step
-- the progress bar.
startProgress
    :: (LabelAmount s)
    => Label s   -- ^ Prefixed label.
    -> Label s   -- ^ Postfixed label.
    -> Integer   -- ^ Total progress bar width in characters.
    -> s         -- ^ Init state
    -> IO (ProgressRef s, ThreadId)
startProgress mkPreLabel mkPostLabel width st = do
    pr  <- buildProgressRef
    tid <- forkIO $ reportProgress pr
    return (pr, tid)
    where
      buildProgressRef = do
        tvSt    <- atomically $ newTVar st
        queue   <- atomically $ newTMQueue
        return $ ProgressRef mkPreLabel mkPostLabel width tvSt queue

-- | Increment the progress bar. Negative values will reverse the progress.
-- Progress will never be negative and will silently stop taking data
-- when it completes.
incProgress :: ProgressRef s -> (s -> s) -> IO ()
incProgress progressRef =
    atomically . writeTMQueue (prQueue progressRef)

reportProgress :: (LabelAmount s) => ProgressRef s -> IO ()
reportProgress pr = do
    continue <- atomically $ updateProgress pr
    renderProgress pr
    when continue $ reportProgress pr

updateProgress :: (LabelAmount s) => ProgressRef s -> STM Bool
updateProgress pr = do
    maybe dontContinue doUpdate =<< readTMQueue (prQueue pr)
    where
      dontContinue = return False
      doUpdate updState = do
        st <- readTVar $ prState pr
        let st1 = updState st
            total = todoAmount st1
            count = doneAmount st1
        let newCount = min total $ max 0 count
        writeTVar (prState pr) st1
        if newCount >= total
          then closeTMQueue (prQueue pr) >> dontContinue
          else return True

renderProgress :: (LabelAmount s) => ProgressRef s -> IO ()
renderProgress pr = do
    st <- atomically $ readTVar $ prState pr
    autoProgressBar
      (prPrefix  pr)
      (prPostfix pr)
      (prWidth   pr)
      st

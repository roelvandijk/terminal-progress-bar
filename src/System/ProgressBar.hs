{-# LANGUAGE NoImplicitPrelude, PackageImports, NamedFieldPuns, RecordWildCards, UnicodeSyntax #-}

module System.ProgressBar
    ( -- * Progress bars
      progressBar
    , mkProgressBar
      -- * Labels
    , Label
    , noLabel
    , msg
    , percentage
    , exact
    -- * auto-printing
    , ProgressRef
    , startProgress
    , incProgress
    ) where

import "base" Control.Monad ( (=<<), (>>), return, when )
import "base" Data.Bool     ( otherwise )
import "base" Data.Function ( ($), (.) )
import "base" Data.List     ( null, length, genericLength, genericReplicate )
import "base" Data.Maybe    ( maybe )
import "base" Data.Ord      ( min, max, (>=) )
import "base" Data.Ratio    ( (%) )
import "base" Data.String   ( String )
import "base" Prelude       ( (+), (-), round, floor, Bool(..) )
import "base" System.IO     ( IO, putStr, putChar )
import "base" Text.Printf   ( printf )
import "base" Text.Show     ( show )
import "base"  Control.Concurrent ( ThreadId, forkIO )
import "stm"  Control.Concurrent.STM ( TVar, readTVar, writeTVar, newTVar, atomically, STM )
import "stm-chans"  Control.Concurrent.STM.TMQueue ( TMQueue, readTMQueue, closeTMQueue, writeTMQueue, newTMQueue )
import "base-unicode-symbols" Data.Bool.Unicode ( (∧) )
import "base-unicode-symbols" Data.Eq.Unicode   ( (≢) )
import "base-unicode-symbols" Prelude.Unicode   ( ℤ, ℚ, (⋅) )


-- | Print a progress bar
--
-- Erases the current line! (by outputting '\r') Does not print a
-- newline '\n'. Subsequent invocations will overwrite the previous
-- output.
--
-- Remember to set the correct buffering mode for stdout:
--
-- > import System.IO ( hSetBuffering, BufferMode(NoBuffering), stdout )
-- > hSetBuffering stdout NoBuffering
progressBar ∷ Label -- ^ Prefixed label.
            → Label -- ^ Postfixed label.
            → ℤ     -- ^ Total progress bar width in characters.
            → ℤ     -- ^ Amount of work completed.
            → ℤ     -- ^ Total amount of work.
            → IO ()
progressBar mkPreLabel mkPostLabel width todo done = do
    putChar '\r'
    putStr $ mkProgressBar mkPreLabel mkPostLabel width todo done

-- | Renders a progress bar
--
-- >>> mkProgressBar (msg "Working") percentage 40 30 100
-- "Working [=======>.................]  30%"
mkProgressBar ∷ Label -- ^ Prefixed label.
              → Label -- ^ Postfixed label.
              → ℤ     -- ^ Total progress bar width in characters.
              → ℤ     -- ^ Amount of work completed.
              → ℤ     -- ^ Total amount of work.
              → String
mkProgressBar mkPreLabel mkPostLabel width todo done =
    printf "%s%s[%s%s%s]%s%s"
           preLabel
           prePad
           (genericReplicate completed '=')
           (if remaining ≢ 0 ∧ completed ≢ 0 then ">" else "")
           (genericReplicate (remaining - if completed ≢ 0 then 1 else 0)
                             '.'
           )
           postPad
           postLabel
  where
    -- Amount of work completed.
    fraction ∷ ℚ
    fraction | done ≢ 0  = todo % done
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
    numCompletedChars ∷ ℚ
    numCompletedChars = fraction ⋅ (effectiveWidth % 1)

    completed, remaining ∷ ℤ
    completed = min effectiveWidth $ floor numCompletedChars
    remaining = effectiveWidth - completed

    preLabel, postLabel ∷ String
    preLabel  = mkPreLabel  todo done
    postLabel = mkPostLabel todo done

    prePad, postPad ∷ String
    prePad  = pad preLabel
    postPad = pad postLabel

    pad ∷ String → String
    pad s | null s    = ""
          | otherwise = " "


-- | A label that can be pre- or postfixed to a progress bar.
type Label = ℤ → ℤ → String

-- | The empty label.
--
-- >>> noLabel 30 100
-- ""
noLabel ∷ Label
noLabel = msg ""

-- | A label consisting of a static string.
--
-- >>> msg "foo" 30 100
-- "foo"
msg ∷ String → Label
msg s _ _ = s

-- | A label which displays the progress as a percentage.
--
-- Constant width property:
-- &#x2200; d t : &#x2115;. d &#x2264; t &#x2192; length (percentage d t) &#x2261; 4
--
-- >>> percentage 30 100
-- " 30%"

-- ∀ d t : ℕ. d ≤ t → length (percentage d t) ≡ 3
percentage ∷ Label
percentage done todo = printf "%3i%%" (round (done % todo ⋅ 100) ∷ ℤ)

-- | A label which displays the progress as a fraction of the total
-- amount of work.
--
-- Equal width property:
-- &#x2200; d&#x2081; d&#x2082; t : &#x2115;. d&#x2081; &#x2264; d&#x2082; &#x2264; t &#x2192; length (exact d&#x2081; t) &#x2261; length (exact d&#x2082; t)
--
-- >>> exact 30 100
-- " 30/100"

-- ∀ d₁ d₂ t : ℕ. d₁ ≤ d₂ ≤ t → length (exact d₁ t) ≡ length (exact d₂ t)
exact ∷ Label
exact done total = printf "%*i/%s" (length totalStr) done totalStr
  where
    totalStr = show total

-- * Auto-Printing Progress

data ProgressRef = ProgressRef { prPrefix    ∷ Label
                               , prPostfix   ∷ Label
                               , prWidth     ∷ ℤ
                               , prCompleted ∷ TVar ℤ
                               , prTotal     ∷ ℤ
                               , prQueue     ∷ TMQueue ℤ }

-- | Start a thread to automatically display progress. Use incProgress to step
-- the progress bar.
startProgress ∷ Label -- ^ Prefixed label.
              → Label -- ^ Postfixed label.
              → ℤ     -- ^ Total progress bar width in characters.
              → ℤ     -- ^ Total amount of work.
              → IO (ProgressRef, ThreadId)
startProgress mkPreLabel mkPostLabel width total = do
    pr  <- buildProgressRef
    tid <- forkIO $ reportProgress pr
    return (pr, tid)
    where
      buildProgressRef = do
        completed <- atomically $ newTVar 0
        queue     <- atomically $ newTMQueue
        return $ ProgressRef mkPreLabel mkPostLabel width completed total queue

-- | Increment the progress bar. Negative values will reverse the progress.
-- Progress will never be negative and will silently stop taking data when it
-- completes.
incProgress ∷ ProgressRef
            → ℤ
            → IO ()
incProgress ProgressRef {prQueue} = atomically . writeTMQueue prQueue

reportProgress ∷ ProgressRef
               → IO ()
reportProgress pr = do
  continue <- atomically $ updateProgress pr
  renderProgress pr
  when continue $ reportProgress pr

updateProgress ∷ ProgressRef
               → STM Bool
updateProgress ProgressRef {prCompleted, prQueue, prTotal} = do
  maybe dontContinue doUpdate =<< readTMQueue prQueue
  where
    dontContinue = return False
    doUpdate countDiff = do
      count <- readTVar prCompleted
      let newCount = min prTotal $ max 0 $ count + countDiff
      writeTVar prCompleted newCount
      if newCount >= prTotal
        then closeTMQueue prQueue >> dontContinue
        else return True
                       
renderProgress ∷ ProgressRef
               → IO ()
renderProgress ProgressRef {..} = do
  completed <- atomically $ readTVar prCompleted
  progressBar prPrefix prPostfix prWidth completed prTotal

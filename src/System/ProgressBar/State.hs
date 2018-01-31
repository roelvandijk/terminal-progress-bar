{-# language PackageImports #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |

The 'Label's in this module have access to a polymorphic state. The
only constrained on the state is that it can be converted a a
'Progress'. This is expressed by the 'HasProgress' class.


-}
module System.ProgressBar.State
    ( -- * Progress bars
      progressBar
    , hProgressBar
    , mkProgressBar
      -- * Options
    , ProgressOptions(..)
    , defProgressOptions
    , ProgressBarWidth(..)
      -- * Progress state
    , Progress(..)
    , HasProgress(..)
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

import "async" Control.Concurrent.Async ( Async, async )
import "base" Control.Monad ( when )
import "base" Data.List     ( genericLength, genericReplicate )
import "base" Data.Ratio    ( (%) )
import "base" System.IO     ( Handle, stderr, hPutChar, hPutStr, hFlush )
import "base" Text.Printf   ( printf )
import "stm"  Control.Concurrent.STM
    ( TVar, readTVar, writeTVar, newTVar, atomically, STM )
import "stm-chans"  Control.Concurrent.STM.TMQueue
    ( TMQueue, readTMQueue, closeTMQueue, writeTMQueue, newTMQueue )
import qualified "terminal-size" System.Console.Terminal.Size as TS

--------------------------------------------------------------------------------

-- | Print a progress bar to 'stderr'
--
-- See 'hProgressBar'.
progressBar :: (HasProgress s) => ProgressOptions s -> s -> IO ()
progressBar = hProgressBar stderr

-- | Print a progress bar to a file handle.
--
-- Erases the current line! (by outputting '\r') Does not print a
-- newline '\n'. Subsequent invocations will overwrite the previous
-- output.
hProgressBar :: (HasProgress s) => Handle -> ProgressOptions s -> s -> IO ()
hProgressBar hndl opts st = do
    opts' <- updateWidth
    hPutChar hndl '\r'
    hPutStr hndl $ mkProgressBar opts' st
    hFlush hndl
  where
    updateWidth =
        case progressOptWidth opts of
          ConstantWidth {} -> pure opts
          TerminalWidth {} -> do
            mbWindow <- TS.size
            pure $ case mbWindow of
              Nothing -> opts
              Just window -> opts{ progressOptWidth = TerminalWidth (TS.width window) }

-- | Renders a progress bar
--
-- >>> mkProgressBar (msg "Working") percentage 40 30 100
-- "Working [=======>.................]  30%"
--
-- Not that this function can not use 'TerminalWidth' because it
-- doesn't use 'IO'. Use 'progressBar' or 'hProgressBar' to get
-- automatic width.
mkProgressBar :: (HasProgress s) => ProgressOptions s -> s -> String
mkProgressBar opts st =
    printf "%s%s%s%s%s%s%s%s%s"
           preLabel
           prePad
           (progressOptOpen opts)
           (concat $ genericReplicate completed $ progressOptDone opts)
           ( if remaining /= 0 && completed /= 0
             then progressOptCurrent opts else ""
           )
           ( concat $ genericReplicate
               (remaining - if completed /= 0 then 1 else 0)
               (progressOptTodo opts)
           )
           (progressOptClose opts)
           postPad
           postLabel
  where
    progress = getProgress st
    todo = progressTodo progress
    done = progressDone progress
    width = getProgressBarWidth $ progressOptWidth opts

    -- Amount of work completed.
    fraction :: Rational
    fraction | todo /= 0 = done % todo
             | otherwise = 0 % 1

    -- Amount of characters available to visualize the progress.
    effectiveWidth = max 0 $ width - usedSpace
    usedSpace =   genericLength (progressOptOpen  opts)
                + genericLength (progressOptClose opts)
                + genericLength preLabel
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
    preLabel  = progressOptPrefix  opts st
    postLabel = progressOptPostfix opts st

    prePad, postPad :: String
    prePad  = pad preLabel
    postPad = pad postLabel

    pad :: String -> String
    pad s | null s    = ""
          | otherwise = " "

-- | Width of progress bar in characters.
data ProgressBarWidth
   = ConstantWidth !Integer
     -- ^ A constant width.
   | TerminalWidth !Integer
     -- ^ Use the entire width of the terminal.
     --
     -- Identical to 'ConstantWidth' if the width of the terminal can
     -- not be determined.

getProgressBarWidth :: ProgressBarWidth -> Integer
getProgressBarWidth (ConstantWidth n) = n
getProgressBarWidth (TerminalWidth n) = n

{- | Options that determine the textual representation of a progress
bar.

The textual representation of a progress bar follows the following template:

\<__prefix__>\<__open__>\<__done__>\<__current__>\<__todo__>\<__close__>\<__postfix__>

Where \<__done__> and \<__todo__> are repeated as often as necessary.

Consider the following progress bar

> "Working [=======>.................]  30%"

This bar can be specified using the following options:

@
'ProgressOptions'
{ 'progressOptOpen'    = \"["
, 'progressOptClose'   = \"]"
, 'progressOptDone'    = \"="
, 'progressOptCurrent' = \">"
, 'progressOptTodo'    = \"."
, 'progressOptPrefix'  = 'msg' \"Working"
, 'progressOptPostfix' = 'percentage'
, 'progressOptWidth'   = 'ConstantWidth' 40
}
@
-}
data ProgressOptions s
   = ProgressOptions
     { progressOptOpen :: !String
       -- ^ Bar opening symbol.
     , progressOptClose :: !String
       -- ^ Bar closing symbol
     , progressOptDone :: !String
       -- ^ Completed work.
     , progressOptCurrent :: !String
       -- ^ Symbol used to denote the current amount of work that has been done.
     , progressOptTodo :: !String
       -- ^ Work not yet completed.
     , progressOptPrefix :: Label s
       -- ^ Prefixed label.
     , progressOptPostfix :: Label s
       -- ^ Postfixed label.
     , progressOptWidth :: !ProgressBarWidth
       -- ^ Total width of the progress bar.
     }

defProgressOptions :: (HasProgress s) => ProgressOptions s
defProgressOptions =
    ProgressOptions
    { progressOptOpen     = "["
    , progressOptClose    = "]"
    , progressOptDone     = "="
    , progressOptCurrent  = ">"
    , progressOptTodo     = "."
    , progressOptPrefix   = noLabel
    , progressOptPostfix  = percentage
    , progressOptWidth    = TerminalWidth 50
    }

-- | State of a progress bar.
data Progress
   = Progress
     { progressDone :: !Integer
       -- ^ Amount of work completed.
     , progressTodo :: !Integer
       -- ^ Total amount of work.
     }

-- | Types that can represent progress.
--
-- Any progress bar state that implements this class can be used by
-- the default label functions.
class HasProgress a where
    getProgress :: a -> Progress

instance HasProgress Progress where
    getProgress = id

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
-- \(\forall d t : \mathbb{N}. d \leq t \rightarrow \texttt{(length \$ percentage \$ Progress d t)} \equiv 4\)
--
-- >>> percentage 30 100
-- " 30%"
--
-- __Note__: if no work is to be done (todo == 0) the percentage will
-- always be 100%.
percentage :: HasProgress s => Label s
percentage s
    | todo == 0 = "100%"
    | otherwise = printf "%3i%%" (round (done % todo * 100) :: Integer)
  where
    done = progressDone progress
    todo = progressTodo progress
    progress = getProgress s

-- | A label which displays the progress as a fraction of the total
-- amount of work.
--
-- Equal width property - the length of the resulting label is a function of the
-- total amount of work:
--
-- \(\forall d_1 d_2 t : \mathbb{N}. d_1 \leq d_2 \leq t \rightarrow \texttt{(length \$ exact \$ Progress d1 t)} \equiv \texttt{(length \$ exact \$ Progress d2 t)}\)
--
-- >>> exact (30, 100)
-- " 30/100"
exact :: HasProgress s => Label s
exact s = printf "%*i/%s" (length totalStr) done totalStr
  where
    totalStr = show todo
    done = progressDone progress
    todo = progressTodo progress
    progress = getProgress s

--------------------------------------------------------------------------------

data ProgressRef s
   = ProgressRef
     { prOptions :: !(ProgressOptions s)
     , prState   :: !(TVar s)
     , prQueue   :: !(TMQueue (s -> s))
     }

-- | Start a thread to automatically display progress. Use incProgress to step
-- the progress bar.
startProgress
    :: (HasProgress s)
    => ProgressOptions s
    -> s -- ^ Initial state.
    -> IO (ProgressRef s, Async ())
startProgress opts st = do
    pr <- buildProgressRef
    a  <- async $ reportProgress pr
    return (pr, a)
    where
      buildProgressRef = do
        tvSt  <- atomically $ newTVar st
        queue <- atomically $ newTMQueue
        return $ ProgressRef opts tvSt queue

-- | Increment the progress bar. Negative values will reverse the progress.
-- Progress will never be negative and will silently stop taking data
-- when it completes.
incProgress :: ProgressRef s -> (s -> s) -> IO ()
incProgress progressRef =
    atomically . writeTMQueue (prQueue progressRef)

reportProgress :: (HasProgress s) => ProgressRef s -> IO ()
reportProgress pr = do
    continue <- atomically $ updateProgress pr
    renderProgress pr
    when continue $ reportProgress pr

updateProgress :: (HasProgress s) => ProgressRef s -> STM Bool
updateProgress pr =
    maybe dontContinue doUpdate =<< readTMQueue (prQueue pr)
  where
    dontContinue = return False
    doUpdate updateState = do
      st <- readTVar $ prState pr
      let newState = updateState st
          progress = getProgress newState
          todo = progressTodo progress
          done = progressDone progress
      let newDone = min todo $ max 0 done
      writeTVar (prState pr) newState
      if newDone >= todo
        then closeTMQueue (prQueue pr) >> dontContinue
        else return True

renderProgress :: (HasProgress s) => ProgressRef s -> IO ()
renderProgress pr = do
    st <- atomically $ readTVar $ prState pr
    progressBar (prOptions pr) st

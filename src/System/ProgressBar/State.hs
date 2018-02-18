{-# language OverloadedStrings #-}
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
import "base" Data.Int      ( Int64 )
import "base" Data.Monoid   ( (<>) )
import "base" Data.Ratio    ( Ratio, (%) )
import "base" System.IO     ( Handle, stderr, hFlush )
import "stm"  Control.Concurrent.STM
    ( TVar, readTVar, writeTVar, newTVar, atomically, STM )
import "stm-chans"  Control.Concurrent.STM.TMQueue
    ( TMQueue, readTMQueue, closeTMQueue, writeTMQueue, newTMQueue )
import qualified "terminal-size" System.Console.Terminal.Size as TS
import qualified "text" Data.Text.Lazy             as TL
import qualified "text" Data.Text.Lazy.Builder     as TLB
import qualified "text" Data.Text.Lazy.Builder.Int as TLB
import qualified "text" Data.Text.Lazy.IO          as TL

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
    TL.hPutStr hndl "\r"
    TL.hPutStr hndl $ mkProgressBar opts' st
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
mkProgressBar :: (HasProgress s) => ProgressOptions s -> s -> TL.Text
mkProgressBar opts st = TL.concat
    [ preLabel
    , prePad
    , progressOptOpen opts
    , TL.replicate completed $ TL.singleton $ progressOptDone opts
    , if remaining /= 0 && completed /= 0
      then TL.singleton $ progressOptCurrent opts
      else ""
    , TL.replicate
        (remaining - if completed /= 0 then 1 else 0)
        (TL.singleton $ progressOptTodo opts)
    , progressOptClose opts
    , postPad
    , postLabel
    ]
  where
    progress = getProgress st
    todo = fromIntegral $ progressTodo progress
    done = fromIntegral $ progressDone progress
    -- Amount of (visible) characters that should be used to display to progress bar.
    width = fromIntegral $ getProgressBarWidth $ progressOptWidth opts

    -- Amount of work completed.
    fraction :: Ratio Int64
    fraction | todo /= 0 = done % todo
             | otherwise = 0 % 1

    -- Amount of characters available to visualize the progress.
    effectiveWidth = max 0 $ width - usedSpace
    usedSpace =   TL.length (progressOptOpen  opts)
                + TL.length (progressOptClose opts)
                + TL.length preLabel
                + TL.length postLabel
                + TL.length prePad
                + TL.length postPad

    -- Number of characters needed to represent the amount of work
    -- that is completed. Note that this can not always be represented
    -- by an integer.
    numCompletedChars :: Ratio Int64
    numCompletedChars = fraction * (effectiveWidth % 1)

    completed, remaining :: Int64
    completed = min effectiveWidth $ floor numCompletedChars
    remaining = effectiveWidth - completed

    preLabel, postLabel :: TL.Text
    preLabel  = progressOptPrefix  opts st
    postLabel = progressOptPostfix opts st

    prePad, postPad :: TL.Text
    prePad  = pad preLabel
    postPad = pad postLabel

    pad :: TL.Text -> TL.Text
    pad s | TL.null s = TL.empty
          | otherwise = TL.singleton ' '

-- | Width of progress bar in characters.
data ProgressBarWidth
   = ConstantWidth !Int
     -- ^ A constant width.
   | TerminalWidth !Int
     -- ^ Use the entire width of the terminal.
     --
     -- Identical to 'ConstantWidth' if the width of the terminal can
     -- not be determined.

getProgressBarWidth :: ProgressBarWidth -> Int
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
, 'progressOptDone'    = \'='
, 'progressOptCurrent' = \'>'
, 'progressOptTodo'    = \'.'
, 'progressOptPrefix'  = 'msg' \"Working"
, 'progressOptPostfix' = 'percentage'
, 'progressOptWidth'   = 'ConstantWidth' 40
}
@
-}
data ProgressOptions s
   = ProgressOptions
     { progressOptOpen :: !TL.Text
       -- ^ Bar opening symbol.
     , progressOptClose :: !TL.Text
       -- ^ Bar closing symbol
     , progressOptDone :: !Char
       -- ^ Completed work.
     , progressOptCurrent :: !Char
       -- ^ Symbol used to denote the current amount of work that has been done.
     , progressOptTodo :: !Char
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
    , progressOptDone     = '='
    , progressOptCurrent  = '>'
    , progressOptTodo     = '.'
    , progressOptPrefix   = noLabel
    , progressOptPostfix  = percentage
    , progressOptWidth    = TerminalWidth 50
    }

-- | State of a progress bar.
data Progress
   = Progress
     { progressDone :: !Int
       -- ^ Amount of work completed.
     , progressTodo :: !Int
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
   = s -- ^ Current progress bar state.
  -> TL.Text -- ^ Resulting label.

-- | The empty label.
--
-- >>> noLabel st
-- ""
noLabel :: Label s
noLabel = msg TL.empty

-- | A label consisting of a static string.
--
-- >>> msg "foo" st
-- "foo"
msg :: TL.Text -> Label s
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
    | otherwise = TL.justifyRight 4 ' ' $ TLB.toLazyText $
                    TLB.decimal (round (done % todo * 100) :: Int)
                    <> TLB.singleton '%'
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
exact s = TL.justifyRight (TL.length todoStr) ' ' doneStr <> "/" <> todoStr
  where
    todoStr = TLB.toLazyText $ TLB.decimal todo
    doneStr = TLB.toLazyText $ TLB.decimal done

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

{-# language DeriveGeneric #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}
{-# language ScopedTypeVariables #-}

module System.ProgressBar
    ( -- * Progress bars
      ProgressBar
    , newProgressBar
    , hNewProgressBar
    , renderProgressBar
    , updateProgress
      -- * Options
    , Style(..)
    , EscapeCode
    , defStyle
    , ProgressBarWidth(..)
      -- * Progress
    , Progress(..)
      -- * Labels
    , Label(..)
    , Timing(..)
    , msg
    , percentage
    , exact
    , elapsedTime
    , remainingTime
    , totalTime
    , renderDuration
    ) where

import "base" Control.Concurrent.MVar ( MVar, newMVar, modifyMVar_)
import "base" Control.Monad ( when )
import "base" Data.Int       ( Int64 )
import "base" Data.Monoid    ( Monoid, mempty )
import "base" Data.Ratio     ( Ratio, (%) )
import "base" Data.Semigroup ( Semigroup, (<>) )
import "base" Data.String    ( IsString, fromString )
import "base" GHC.Generics   ( Generic )
import "base" System.IO      ( Handle, stderr, hFlush )
import "deepseq" Control.DeepSeq ( NFData, rnf )
import qualified "terminal-size" System.Console.Terminal.Size as TS
import qualified "text" Data.Text.Lazy             as TL
import qualified "text" Data.Text.Lazy.Builder     as TLB
import qualified "text" Data.Text.Lazy.Builder.Int as TLB
import qualified "text" Data.Text.Lazy.IO          as TL
import "time" Data.Time.Clock ( UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime )

--------------------------------------------------------------------------------

data ProgressBar s
   = ProgressBar
     { pbStyle :: !(Style s)
     , pbStateMv :: !(MVar (State s))
     , pbRefreshDelay :: !Double
     , pbStartTime :: !UTCTime
     , pbHandle :: !Handle
     }

instance (NFData s) => NFData (ProgressBar s) where
    rnf pb =  pbStyle pb
        `seq` pbStateMv pb
        `seq` pbRefreshDelay pb
        `seq` pbStartTime pb
        -- pbHandle is ignored
        `seq` ()

data State s
   = State
     { stProgress :: !(Progress s)
     , stRenderTime :: !UTCTime
     }

-- | State of a progress bar.
data Progress s
   = Progress
     { progressDone :: !Int
       -- ^ Amount of work completed.
     , progressTodo :: !Int
       -- ^ Total amount of work.
     , progressCustom :: !s
       -- ^ A custom value which can be used by custom labels.
       -- Simply use '()' if you do not need custom progress values.
     }

progressCompleted :: Progress s -> Bool
progressCompleted p = progressDone p >= progressTodo p

newProgressBar
    :: Style s
    -> Double -- ^ Maximum refresh rate in Hertz.
    -> Progress s -- ^ Initial progress.
    -> IO (ProgressBar s)
newProgressBar = hNewProgressBar stderr

hNewProgressBar
    :: Handle
    -> Style s
    -> Double -- ^ Maximum refresh rate in Hertz.
    -> Progress s -- ^ Initial progress.
    -> IO (ProgressBar s)
hNewProgressBar hndl style maxRefreshRate initProgress = do
    style' <- updateWidth style

    startTime <- getCurrentTime
    hPutProgressBar hndl style' initProgress (Timing startTime startTime)

    stateMv <- newMVar
      State
      { stProgress   = initProgress
      , stRenderTime = startTime
      }
    pure ProgressBar
         { pbStyle = style'
         , pbStateMv = stateMv
         , pbRefreshDelay = recip maxRefreshRate
         , pbStartTime = startTime
         , pbHandle = hndl
         }

updateWidth :: Style s -> IO (Style s)
updateWidth style =
    case styleWidth style of
      ConstantWidth {} -> pure style
      TerminalWidth {} -> do
        mbWindow <- TS.size
        pure $ case mbWindow of
          Nothing -> style
          Just window -> style{ styleWidth = TerminalWidth (TS.width window) }

updateProgress
    :: forall s. ProgressBar s -> (Progress s -> Progress s) -> IO ()
updateProgress progressBar f = do
    updateTime <- getCurrentTime
    modifyMVar_ (pbStateMv progressBar) $ renderAndUpdate updateTime
  where
    renderAndUpdate :: UTCTime -> State s -> IO (State s)
    renderAndUpdate updateTime state = do
        when shouldRender $
          hPutProgressBar hndl (pbStyle progressBar) newProgress timing
        pure State
             { stProgress = newProgress
             , stRenderTime = if shouldRender then updateTime else stRenderTime state
             }
      where
        timing = Timing
                 { timingStart = pbStartTime progressBar
                 , timingLastUpdate = updateTime
                 }

        shouldRender = not tooFast || completed
        tooFast = secSinceLastRender <= pbRefreshDelay progressBar
        completed = progressCompleted newProgress

        newProgress = f $ stProgress state

        -- Amount of time that passed since last render, in seconds.
        secSinceLastRender :: Double
        secSinceLastRender = realToFrac $ diffUTCTime updateTime (stRenderTime state)

    hndl = pbHandle progressBar

hPutProgressBar :: Handle -> Style s -> Progress s -> Timing -> IO ()
hPutProgressBar hndl style progress timing = do
    TL.hPutStr hndl "\r"
    TL.hPutStr hndl $ renderProgressBar style progress timing
    when (progressCompleted progress) $ TL.hPutStr hndl "\n"
    hFlush hndl

-- | Renders a progress bar
--
-- >>> renderProgressBar (msg "Working") percentage 40 30 100
-- "Working [=======>.................]  30%"
--
-- Not that this function can not use 'TerminalWidth' because it
-- doesn't use 'IO'. Use 'progressBar' or 'hProgressBar' to get
-- automatic width.
renderProgressBar :: Style s -> Progress s -> Timing -> TL.Text
renderProgressBar style progress timing = TL.concat
    [ styleEscapePrefix style progress
    , prefixLabel
    , prefixPad
    , styleEscapeOpen style progress
    , styleOpen style
    , styleEscapeDone style progress
    , TL.replicate completed $ TL.singleton $ styleDone style
    , styleEscapeCurrent style progress
    , if remaining /= 0 && completed /= 0
      then TL.singleton $ styleCurrent style
      else ""
    , styleEscapeTodo style progress
    , TL.replicate
        (remaining - if completed /= 0 then 1 else 0)
        (TL.singleton $ styleTodo style)
    , styleEscapeClose style progress
    , styleClose style
    , styleEscapePostfix style progress
    , postfixPad
    , postfixLabel
    ]
  where
    todo = fromIntegral $ progressTodo progress
    done = fromIntegral $ progressDone progress
    -- Amount of (visible) characters that should be used to display to progress bar.
    width = fromIntegral $ getProgressBarWidth $ styleWidth style

    -- Amount of work completed.
    fraction :: Ratio Int64
    fraction | todo /= 0 = done % todo
             | otherwise = 0 % 1

    -- Amount of characters available to visualize the progress.
    effectiveWidth = max 0 $ width - usedSpace
    -- Amount of printing characters needed to visualize everything except the bar .
    usedSpace =   TL.length (styleOpen  style)
                + TL.length (styleClose style)
                + TL.length prefixLabel
                + TL.length postfixLabel
                + TL.length prefixPad
                + TL.length postfixPad

    -- Number of characters needed to represent the amount of work
    -- that is completed. Note that this can not always be represented
    -- by an integer.
    numCompletedChars :: Ratio Int64
    numCompletedChars = fraction * (effectiveWidth % 1)

    completed, remaining :: Int64
    completed = min effectiveWidth $ floor numCompletedChars
    remaining = effectiveWidth - completed

    prefixLabel, postfixLabel :: TL.Text
    prefixLabel  = runLabel (stylePrefix  style) progress timing
    postfixLabel = runLabel (stylePostfix style) progress timing

    prefixPad, postfixPad :: TL.Text
    prefixPad  = pad prefixLabel
    postfixPad = pad postfixLabel

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
     deriving (Generic)

instance NFData ProgressBarWidth

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

This bar can be specified using the following style:

@
'Style'
{ 'styleOpen'    = \"["
, 'styleClose'   = \"]"
, 'styleDone'    = \'='
, 'styleCurrent' = \'>'
, 'styleTodo'    = \'.'
, 'stylePrefix'  = 'msg' \"Working"
, 'stylePostfix' = 'percentage'
, 'styleWidth'   = 'ConstantWidth' 40
, 'styleEscapeOpen'    = const 'TL.empty'
, 'styleEscapeClose'   = const 'TL.empty'
, 'styleEscapeDone'    = const 'TL.empty'
, 'styleEscapeCurrent' = const 'TL.empty'
, 'styleEscapeTodo'    = const 'TL.empty'
, 'styleEscapePrefix'  = const 'TL.empty'
, 'styleEscapePostfix' = const 'TL.empty'
}
@
-}
data Style s
   = Style
     { styleOpen :: !TL.Text
       -- ^ Bar opening symbol.
     , styleClose :: !TL.Text
       -- ^ Bar closing symbol
     , styleDone :: !Char
       -- ^ Completed work.
     , styleCurrent :: !Char
       -- ^ Symbol used to denote the current amount of work that has been done.
     , styleTodo :: !Char
       -- ^ Work not yet completed.
     , stylePrefix :: Label s
       -- ^ Prefixed label.
     , stylePostfix :: Label s
       -- ^ Postfixed label.
     , styleWidth :: !ProgressBarWidth
       -- ^ Total width of the progress bar.
     , styleEscapeOpen :: EscapeCode s
       -- ^ Escape code printed just before the 'styleOpen' symbol.
     , styleEscapeClose :: EscapeCode s
       -- ^ Escape code printed just before the 'styleClose' symbol.
     , styleEscapeDone :: EscapeCode s
       -- ^ Escape code printed just before the first 'styleDone' character.
     , styleEscapeCurrent :: EscapeCode s
       -- ^ Escape code printed just before the 'styleCurrent' character.
     , styleEscapeTodo :: EscapeCode s
       -- ^ Escape code printed just before the first 'styleTodo' character.
     , styleEscapePrefix :: EscapeCode s
       -- ^ Escape code printed just before the 'stylePrefix' label.
     , styleEscapePostfix :: EscapeCode s
       -- ^ Escape code printed just before the 'stylePostfix' label.
     } deriving (Generic)

instance (NFData s) => NFData (Style s)

-- | An escape code is a sequence of bytes which the terminal looks
-- for and interprets as commands, not as character codes.
--
-- It is vital that the output of this function, when send to the
-- terminal, does not result in characters being drawn.
type EscapeCode s
   = Progress s -- ^ Current progress bar state.
  -> TL.Text -- ^ Resulting escape code. Must be non-printable.

-- | A default style.
--
-- You can override some fields of the default instead of specifying
-- all the fields of a 'Style' record.
--
-- The default does not use any escape sequences.
defStyle :: Style s
defStyle =
    Style
    { styleOpen          = "["
    , styleClose         = "]"
    , styleDone          = '='
    , styleCurrent       = '>'
    , styleTodo          = '.'
    , stylePrefix        = mempty
    , stylePostfix       = percentage
    , styleWidth         = TerminalWidth 50
    , styleEscapeOpen    = const TL.empty
    , styleEscapeClose   = const TL.empty
    , styleEscapeDone    = const TL.empty
    , styleEscapeCurrent = const TL.empty
    , styleEscapeTodo    = const TL.empty
    , styleEscapePrefix  = const TL.empty
    , styleEscapePostfix = const TL.empty
    }

-- | A label that can be pre- or postfixed to a progress bar.
newtype Label s = Label{ runLabel :: Progress s -> Timing -> TL.Text } deriving (NFData)

instance Semigroup (Label s) where
    Label f <> Label g = Label $ \p t -> f p t <> g p t

instance Monoid (Label s) where
    mempty = msg TL.empty
    mappend = (<>)

instance IsString (Label s) where
    fromString = msg . TL.pack

data Timing
   = Timing
     { timingStart      :: !UTCTime
     , timingLastUpdate :: !UTCTime
     }

-- | A label consisting of a static string.
--
-- >>> msg "foo" st
-- "foo"
msg :: TL.Text -> Label s
msg s = Label $ \_ _ -> s

-- | A label which displays the progress as a percentage.
--
-- >>> runLabel $ percentage (Progress 30 100 ()) someTiming
-- " 30%"
--
-- __Note__: if no work is to be done (todo == 0) the percentage will
-- always be 100%.
percentage :: Label s
percentage = Label render
  where
    render progress _timing
      | todo == 0 = "100%"
      | otherwise = TL.justifyRight 4 ' ' $ TLB.toLazyText $
                      TLB.decimal (round (done % todo * 100) :: Int)
                      <> TLB.singleton '%'
      where
        done = progressDone progress
        todo = progressTodo progress

-- | A label which displays the progress as a fraction of the total
-- amount of work.
--
-- Equal width property - the length of the resulting label is a function of the
-- total amount of work:
--
-- >>> runLabel $ exact (Progress 30 100 ()) someTiming
-- " 30/100"
exact :: Label s
exact = Label render
  where
    render progress _timing =
        TL.justifyRight (TL.length todoStr) ' ' doneStr <> "/" <> todoStr
      where
        todoStr = TLB.toLazyText $ TLB.decimal todo
        doneStr = TLB.toLazyText $ TLB.decimal done

        done = progressDone progress
        todo = progressTodo progress

-- | A label which displays the amount of time that has elapsed.
--
-- Time starts when a progress bar is created.
--
-- The user must supply a function which actually renders the amount
-- of time that has elapsed. You can use 'renderDuration' or
-- @formatTime@ from time >= 1.9.
elapsedTime
    :: (NominalDiffTime -> TL.Text)
    -> Label s
elapsedTime formatNDT = Label render
  where
    render _progress timing = formatNDT dt
      where
        dt :: NominalDiffTime
        dt = diffUTCTime (timingLastUpdate timing) (timingStart timing)

-- | Displays the estimated remaining time until all work is done.
--
-- Tells you how much longer some task will take.
--
-- This label uses a really simple estimation algorithm. It assumes
-- progress is linear. To prevent nonsense results it won't estimate
-- remaining time until at least 1 second of work has been done.
--
-- When it refuses to estimate the remaining time it will show an
-- alternative message instead.
--
-- The user must supply a function which actually renders the amount
-- of time that has elapsed. You can use 'renderDuration' or
-- @formatTime@ from time >= 1.9.
remainingTime
    :: (NominalDiffTime -> TL.Text)
    -> TL.Text
       -- ^ Alternative message when remaining time can't be
       -- calculated (yet).
    -> Label s
remainingTime formatNDT altMsg = Label render
  where
    render progress timing
        | dt > 1 = formatNDT estimatedRemainingTime
        | progressDone progress <= 0 = altMsg
        | otherwise = altMsg
      where
        estimatedRemainingTime = estimatedTotalTime - dt
        estimatedTotalTime = dt * recip progressFraction

        progressFraction :: NominalDiffTime
        progressFraction
          | progressTodo progress <= 0 = 1
          | otherwise = fromIntegral (progressDone progress)
                      / fromIntegral (progressTodo progress)

        dt :: NominalDiffTime
        dt = diffUTCTime (timingLastUpdate timing) (timingStart timing)

-- | Displays the estimated total time a task will take.
--
-- This label uses a really simple estimation algorithm. It assumes
-- progress is linear. To prevent nonsense results it won't estimate
-- the total time until at least 1 second of work has been done.
--
-- When it refuses to estimate the total time it will show an
-- alternative message instead.
--
-- The user must supply a function which actually renders the total
-- amount of time that a task will take. You can use 'renderDuration'
-- or @formatTime@ from time >= 1.9.
totalTime
    :: (NominalDiffTime -> TL.Text)
    -> TL.Text
       -- ^ Alternative message when total time can't be calculated
       -- (yet).
    -> Label s
totalTime formatNDT altMsg = Label render
  where
    render progress timing
        | dt > 1 = formatNDT estimatedTotalTime
        | progressDone progress <= 0 = altMsg
        | otherwise = altMsg
      where
        estimatedTotalTime = dt * recip progressFraction

        progressFraction :: NominalDiffTime
        progressFraction
          | progressTodo progress <= 0 = 1
          | otherwise = fromIntegral (progressDone progress)
                      / fromIntegral (progressTodo progress)

        dt :: NominalDiffTime
        dt = diffUTCTime (timingLastUpdate timing) (timingStart timing)

-- | Show amount of time.
--
-- > renderDuration (fromInteger 42)
-- 42
--
-- > renderDuration (fromInteger $ 5 * 60 + 42)
-- 05:42
--
-- > renderDuration (fromInteger $ 8 * 60 * 60 + 5 * 60 + 42)
-- 08:05:42
--
-- Use the time >= 1.9 package to get a formatTime function which
-- accepts 'NominalDiffTime'.
renderDuration :: NominalDiffTime -> TL.Text
renderDuration dt = hTxt <> mTxt <> sTxt
  where
    hTxt | h == 0 = mempty
         | otherwise = renderDecimal h <> ":"
    mTxt | m == 0 = mempty
         | otherwise = renderDecimal m <> ":"
    sTxt = renderDecimal s

    (h, hRem) = ts   `quotRem` 3600
    (m, s   ) = hRem `quotRem`   60

    -- Total amount of seconds
    ts :: Int
    ts = round dt

    renderDecimal n = TL.justifyRight 2 '0' $ TLB.toLazyText $ TLB.decimal n

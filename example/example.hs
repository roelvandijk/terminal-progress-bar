{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main where

import "ansi-terminal" System.Console.ANSI.Codes
    ( setSGRCode, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..) )
import qualified "async" Control.Concurrent.Async as Async
import "base" Control.Concurrent ( threadDelay )
import "base" Data.Foldable ( traverse_, for_ )
import "base" Data.Ratio ( (%) )
import "base" Data.Traversable ( for )
import "random" System.Random ( randomRIO )
import "terminal-progress-bar" System.ProgressBar
import qualified "text" Data.Text.Lazy as TL

main :: IO ()
main = do
    putStrLn "Simple progressBar:"
    example 60 25000
    putStrLn ""

    putStrLn "Incrementing process with steps of 1:"
    exampleAsync 60 25000
    putStrLn ""

    putStrLn "Multiple threads increasing progress:"
    exampleAsync2 25000
    putStrLn ""

    putStrLn "Colorful progressBar (fixed colors):"
    exampleColorful 100 25000
    putStrLn ""

    putStrLn "Colorful progressBar (changing colors):"
    exampleColorful2 100 25000
    putStrLn ""

    putStrLn "Really long task. Feel free to kill the executable with CTRL-C or similar."
    example 100000 250000
    putStrLn ""

exampleStyle :: Style ()
exampleStyle =
    defStyle
    { stylePrefix  = percentage
    , stylePostfix = exact <> " " <> elapsedTime renderDuration <> "/" <> totalTime renderDuration "..."
    , styleWidth   = TerminalWidth (13 + 60)
    }

example :: Int -> Int -> IO ()
example todo delay = do
    pb <- newProgressBar exampleStyle 30 (Progress 0 todo ())
    for_ [0 .. todo] $ \done -> do
      updateProgress pb $ \p -> p{progressDone = done}
      threadDelay delay
    putStrLn ""

exampleAsync :: Int -> Int -> IO ()
exampleAsync todo delay = do
    pb <- newProgressBar exampleStyle 30 (Progress 0 todo ())
    for_ [1 .. todo] $ \_done -> do
      incProgress pb 1
      threadDelay delay
    putStrLn ""

exampleAsync2 :: Int -> IO ()
exampleAsync2 delay = do
    pb <- newProgressBar exampleStyle 30 (Progress 0 todo ())
    -- Spawn some threads which each increment progress a bit.
    threads <- for [1 .. numThreads] $ \_ ->
      Async.async $
        for_ [1 .. progressPerThread] $ \_ -> do
          incProgress pb 1
          d <- randomRIO (delay * numThreads, 2 * delay * numThreads)
          threadDelay d

    -- Wait until the task is completed.
    traverse_ Async.wait threads

    putStrLn ""
  where
    todo :: Int
    todo = fromIntegral $ numThreads * progressPerThread

    numThreads :: Int
    numThreads = 10

    progressPerThread :: Int
    progressPerThread = 10

colorStyle :: Style ()
colorStyle =
    exampleStyle
    { styleDone          = '▇'
    , styleCurrent       = '▶'
    , styleTodo          = ' '
    , styleOpen          = ""
    , styleClose         = ""
    , styleEscapeDone    = const $ setSGRCodeText [SetColor Foreground Dull Green]
    , styleEscapePostfix = const $ setSGRCodeText [Reset]
    }

exampleColorful :: Int -> Int -> IO ()
exampleColorful todo delay = do
    pb <- newProgressBar colorStyle 30 (Progress 0 todo ())
    for_ [0 .. todo] $ \done -> do
      updateProgress pb $ \p -> p{progressDone = done}
      threadDelay delay
    putStrLn ""

colorStyle2 :: Style ()
colorStyle2 =
    colorStyle{ styleEscapeDone = setSGRCodeText . progressColor }
  where
    progressColor :: Progress () -> [SGR]
    progressColor (Progress done todo ())
        | r >= 90 % 100 = [SetColor Foreground Dull  Green]
        | r >= 50 % 100 = [SetColor Foreground Vivid Yellow]
        | r >= 25 % 100 = [SetColor Foreground Vivid Red]
        | otherwise     = [SetColor Foreground Dull  Red]
      where
        r = done % todo

exampleColorful2 :: Int -> Int -> IO ()
exampleColorful2 todo delay = do
    pb <- newProgressBar colorStyle2 30 (Progress 0 todo ())
    for_ [0 .. todo] $ \done -> do
      updateProgress pb $ \p -> p{progressDone = done}
      threadDelay delay
    putStrLn ""

setSGRCodeText :: [SGR] -> TL.Text
setSGRCodeText = TL.pack . setSGRCode

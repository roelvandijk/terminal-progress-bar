{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main where

import "ansi-terminal" System.Console.ANSI.Codes
    ( setSGRCode, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..) )
import "async" Control.Concurrent.Async ( async, wait )
import "base" Control.Concurrent ( threadDelay )
import "base" Control.Monad ( forM_ )
import "base" Data.Functor ( void )
import "base" Data.Ratio ( (%) )
import "random" System.Random ( randomRIO )
import "terminal-progress-bar" System.ProgressBar
import qualified "text" Data.Text.Lazy as TL

main :: IO ()
main = do
    putStrLn "Simple direct progressBar:"
    example 60 25000
    putStrLn ""

    putStrLn "ProgressBar with separate token to increase progress:"
    exampleAsync 60 25000
    putStrLn ""

    putStrLn "ProgressBar with multiple threads increasing progress:"
    exampleAsync2 25000
    putStrLn ""

    putStrLn "Colorful progressBar (fixed colors):"
    exampleColorful 100 25000
    putStrLn ""

    putStrLn "Colorful progressBar (changing colors):"
    exampleColorful2 100 25000
    putStrLn ""

exampleOptions :: ProgressOptions Progress
exampleOptions =
    defProgressOptions
    { progressOptPrefix  = percentage
    , progressOptPostfix = exact
    , progressOptWidth   = TerminalWidth (13 + 60)
    }

example :: Int -> Int -> IO ()
example todo delay = do
    forM_ [0 .. todo] $ \done -> do
      progressBar exampleOptions (Progress done todo)
      threadDelay delay
    putStrLn ""

exampleAsync :: Int -> Int -> IO ()
exampleAsync todo delay = do
    (pr, a) <- startProgress exampleOptions (Progress 0 todo)
    forM_ [1 .. todo] $ \_done -> do
      updateProgress pr 1
      threadDelay delay
    wait a
    putStrLn ""

exampleAsync2 :: Int -> IO ()
exampleAsync2 delay = do
    (pr, a) <- startProgress exampleOptions (Progress 0 todo)
    -- Spawn some threads which each increment progress a bit.
    forM_ [1 .. numThreads] $ \_ ->
      void $ async $
        forM_ [1 .. progressPerThread] $ \_ -> do
          updateProgress pr 1
          d <- randomRIO (delay * numThreads, 2 * delay * numThreads)
          threadDelay d

    -- Wait until the task is completed.
    wait a
    putStrLn ""
  where
    todo :: Int
    todo = fromIntegral $ numThreads * progressPerThread

    numThreads :: Int
    numThreads = 10

    progressPerThread :: Int
    progressPerThread = 10

colorOptions :: ProgressOptions Progress
colorOptions =
    exampleOptions
    { progressOptDone          = '▇'
    , progressOptCurrent       = '▶'
    , progressOptTodo          = ' '
    , progressOptOpen          = ""
    , progressOptClose         = ""
    , progressOptEscapeDone    = const $ setSGRCodeText [SetColor Foreground Dull Green]
    , progressOptEscapePostfix = const $ setSGRCodeText [Reset]
    }

exampleColorful :: Int -> Int -> IO ()
exampleColorful todo delay = do
    forM_ [0 .. todo] $ \done -> do
      progressBar colorOptions (Progress done todo)
      threadDelay delay
    putStrLn ""

colorOptions2 :: ProgressOptions Progress
colorOptions2 =
    colorOptions{ progressOptEscapeDone = setSGRCodeText . progressColor }
  where
    progressColor :: Progress -> [SGR]
    progressColor (Progress done todo)
        | r >= 90 % 100 = [SetColor Foreground Dull  Green]
        | r >= 50 % 100 = [SetColor Foreground Vivid Yellow]
        | r >= 25 % 100 = [SetColor Foreground Vivid Red]
        | otherwise     = [SetColor Foreground Dull  Red]
      where
        r = done % todo

exampleColorful2 :: Int -> Int -> IO ()
exampleColorful2 todo delay = do
    forM_ [0 .. todo] $ \done -> do
      progressBar colorOptions2 (Progress done todo)
      threadDelay delay
    putStrLn ""

setSGRCodeText :: [SGR] -> TL.Text
setSGRCodeText = TL.pack . setSGRCode

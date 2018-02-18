{-# LANGUAGE PackageImports #-}

module Main where

import "async" Control.Concurrent.Async ( async, wait )
import "base" Control.Concurrent ( threadDelay )
import "base" Control.Monad ( forM_ )
import "base" Data.Functor ( void )
import "random" System.Random ( randomRIO )
import "terminal-progress-bar" System.ProgressBar

main :: IO ()
main = do
    example       60 25000
    exampleAsync  60 25000
    exampleAsync2    25000

exampleOptions :: ProgressOptions Progress
exampleOptions =
    defProgressOptions
    { progressOptPrefix  = percentage
    , progressOptPostfix = exact
    , progressOptWidth   = TerminalWidth (13 + 60)
    }

example :: Int -> Int -> IO ()
example todo delay = do
    forM_ [1 .. todo] $ \done -> do
      progressBar exampleOptions (Progress done todo)
      threadDelay delay
    putStrLn ""

exampleAsync :: Int -> Int -> IO ()
exampleAsync todo delay = do
    (pr, a) <- startProgress exampleOptions (Progress 0 todo)
    forM_ [1 .. todo] $ \_done -> do
      incProgress pr 13
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
          incProgress pr 1
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

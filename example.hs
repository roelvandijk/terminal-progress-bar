{-# LANGUAGE PackageImports #-}

module Main where

import "base" Control.Concurrent ( threadDelay )
import "base" Control.Monad ( forM_ )
import "terminal-progress-bar" System.ProgressBar
    ( autoProgressBar, percentage, exact, startProgress, incProgress )

main :: IO ()
main = do
  example  60 (13 + 60) 25000
  example' 60 (13 + 60) 25000

example :: Integer -> Integer -> Int -> IO ()
example t w delay = do
    forM_ [1..t] $ \d -> do
      autoProgressBar percentage exact w d t
      threadDelay delay
    putStrLn ""

example' :: Integer -> Integer -> Int -> IO ()
example' t w delay = do
    (pr, _) <- startProgress percentage exact w t
    forM_ [1..t] $ \_d -> do
      incProgress pr 1
      threadDelay delay
    putStrLn ""

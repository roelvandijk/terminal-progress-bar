{-# LANGUAGE PackageImports #-}

module Main where

import "base" Control.Concurrent ( threadDelay )
import "base" Control.Monad ( forM_ )
import "terminal-progress-bar" System.ProgressBar

main :: IO ()
main = do
  example  60 (13 + 60) 25000
  example' 60 (13 + 60) 25000

example :: Integer -> Integer -> Int -> IO ()
example todo w delay = do
    forM_ [1 .. todo] $ \done -> do
      autoProgressBar percentage exact w $ Progress done todo
      threadDelay delay
    putStrLn ""

example' :: Integer -> Integer -> Int -> IO ()
example' todo w delay = do
    (pr, _) <- startProgress percentage exact w $ Progress 0 todo
    forM_ [1 .. todo] $ \_done -> do
      incProgress pr 1
      threadDelay delay
    putStrLn ""

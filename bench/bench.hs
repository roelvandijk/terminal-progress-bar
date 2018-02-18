{-# language PackageImports #-}
module Main where

import "base" Data.Monoid ( (<>) )
import "criterion" Criterion.Main
import "terminal-progress-bar" System.ProgressBar


main :: IO ()
main = defaultMain
       [ progressBarBenchmark  10   0
       , progressBarBenchmark  10  50
       , progressBarBenchmark  10 100
       , progressBarBenchmark 100   0
       , progressBarBenchmark 100  50
       , progressBarBenchmark 100 100
       , progressBarBenchmark 200   0
       , progressBarBenchmark 200  50
       , progressBarBenchmark 200 100
       , labelBenchmark "percentage" percentage (Progress   0 100)
       , labelBenchmark "percentage" percentage (Progress  50 100)
       , labelBenchmark "percentage" percentage (Progress 100 100)
       , labelBenchmark "exact"      exact      (Progress   0 100)
       , labelBenchmark "exact"      exact      (Progress  50 100)
       , labelBenchmark "exact"      exact      (Progress 100 100)
       ]

progressBarBenchmark :: Int -> Int -> Benchmark
progressBarBenchmark width done =
    bench name $ whnfIO $
      progressBar
        defProgressOptions
        { progressOptWidth = ConstantWidth width
        }
        Progress
        { progressDone = done
        , progressTodo = 100
        }
  where
    name = "progressBar/default - "
           <> show width <> " wide - progress " <> show done <> " % 100"

labelBenchmark :: String -> Label -> Progress -> Benchmark
labelBenchmark labelName label progress = bench name $ nf label progress
  where
    name = "label/" <> labelName <> " "
           <> show (progressDone progress) <> " % "
           <> show (progressTodo progress)

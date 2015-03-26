module Main (main) where

import qualified Dicom.DictionaryBench
-- HASKELETON: import qualified New.ModuleBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain
    [ bgroup "Dicom.Dictionary" Dicom.DictionaryBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]

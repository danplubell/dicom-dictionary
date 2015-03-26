module Dicom.DictionaryBench (benchmarks) where

import Dicom.Dictionary

import Criterion

benchmarks :: [Benchmark]
benchmarks =
    [ bench "main" (nfIO main)
    ]

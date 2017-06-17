{-# language OverloadedStrings #-}

import Parser

import Prelude hiding (FilePath)
import Criterion (Benchmark)

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Prelude
import qualified Criterion
import qualified Criterion.Main

drvFile :: Prelude.FilePath
drvFile = "data/2rq2qqngncvvdvh0f32kwi1d2gcn32k6-ghc-8.0.2-with-packages.drv"

benchmarks :: [Benchmark]
benchmarks =
    [ Criterion.Main.env
        (Data.Text.Lazy.IO.readFile drvFile)
        bench0
    ]
  where
    bench0 example =
        Criterion.bench "example" (Criterion.nf parseExample example)

    parseExample =
        Data.Attoparsec.Text.Lazy.parse parseDerivation

benchMain :: IO ()
benchMain = Criterion.Main.defaultMain benchmarks

main :: IO ()
main = benchMain

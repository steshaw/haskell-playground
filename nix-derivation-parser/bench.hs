{-# LANGUAGE OverloadedStrings #-}

import Parser

import Criterion (Benchmark)
import Prelude hiding (FilePath)

import qualified Criterion
import qualified Criterion.Main
import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Prelude

drvFile :: Prelude.FilePath
drvFile = "data/2rq2qqngncvvdvh0f32kwi1d2gcn32k6-ghc-8.0.2-with-packages.drv"

benchmarks :: [Benchmark]
benchmarks = [Criterion.Main.env (Data.Text.Lazy.IO.readFile drvFile) bench0]
  where
    bench0 example =
      Criterion.bench "example" (Criterion.nf parseExample example)
    parseExample = Data.Attoparsec.Text.Lazy.parse parseDerivation

main :: IO ()
main = Criterion.Main.defaultMain benchmarks

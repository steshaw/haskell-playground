{-# LANGUAGE OverloadedStrings #-}

import Parser

import Criterion (Benchmark)
import Prelude hiding (FilePath)

import qualified Criterion
import qualified Criterion.Main
import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.ByteString.Lazy.Char8 as SIO
import qualified Prelude

drvFile :: Prelude.FilePath
drvFile = "data/2rq2qqngncvvdvh0f32kwi1d2gcn32k6-ghc-8.0.2-with-packages.drv"

benchmarks :: [Benchmark]
benchmarks = [Criterion.Main.env (SIO.readFile drvFile) bench0]
  where
    bench0 example =
      Criterion.bench "example" (Criterion.nf parseExample example)
    parseExample = P.parse parseDerivation

main :: IO ()
main = Criterion.Main.defaultMain benchmarks

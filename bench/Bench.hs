{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.URI.File
import Test.Tasty.Bench

main :: IO ()
main = defaultMain benchMark


benchMark :: [Benchmark]
benchMark =
    [ bgroup "parseFileURI StrictPosix"
      [ bench "file://hostname/path/to/file/foo/bar/baz"
      $ whnf (parseFileURI StrictPosix) "file://user@hostname/path/to/file/foo/bar/baz"
      ]
    , bgroup "parseFileURI ExtendedPosix"
      [ bench "file://user@hostname/path/to/file/foo/bar/baz"
      $ whnf (parseFileURI ExtendedPosix) "file://user@hostname/path/to/file/foo/bar/baz"
      ]
    , bgroup "parseFileURI ExtendedWindows"
      [ bench "file://user@hostname/c|/path/to/file"
      $ whnf (parseFileURI ExtendedWindows) "file://user@hostname/c|/path/to/file"
      , bench "file://///host.example.com/path/to/file"
      $ whnf (parseFileURI ExtendedWindows) "file://///host.example.com/path/to/file"
      ]
    ]


{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.URI.File

import Data.ByteString
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  testGroup
    "System.URI.File"
    [ parseFileURIStrict
    ]

parseFileURIStrict :: TestTree
parseFileURIStrict =
  testGroup
    "parseFileURI (Strict)"
    [ parseTestURI Strict "file:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/path/to/file" })
    , parseTestURI Strict "file:///path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/path/to/file" })
    , parseTestURI Strict "file://hostname/path/to/file"
      (Right FileURI { fileAuth = Just "hostname", filePath = "/path/to/file" })
    , parseTestURI Strict "file://localhost/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/path/to/file" })
    , parseTestURI Strict "http://localhost/path/to/file"
      (Left "string")
    , parseTestURI Strict "/path/to/file"
      (Left "string")
    ]


parseTestURI :: ParseConfig -> ByteString -> Either String FileURI -> TestTree
parseTestURI cfg s r = testCase (B8.unpack s) $ parseFileURI cfg s @?= r


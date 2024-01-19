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
    [ parseFileURIStrictPosix
    , parseFileURIExtendedPosix
    , parseFileURIExtendedWindows
    ]

parseFileURIStrictPosix :: TestTree
parseFileURIStrictPosix =
  testGroup
    "parseFileURI (StrictPosix)"
    [ parseTestURI StrictPosix "file:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/path/to/file" })
    , parseTestURI StrictPosix "file:///path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/path/to/file" })
    , parseTestURI StrictPosix "file://hostname/path/to/file"
      (Right FileURI { fileAuth = Just "hostname", filePath = "/path/to/file" })
    , parseTestURI StrictPosix "file://localhost/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/path/to/file" })
    , parseTestURI StrictPosix "http://localhost/path/to/file"
      (Left "string")
    , parseTestURI StrictPosix "/path/to/file"
      (Left "string")

    , parseTestURI StrictPosix "file:///c:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/c:/path/to/file" })
    , parseTestURI StrictPosix "file:/c:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/c:/path/to/file" })
    , parseTestURI StrictPosix "file:c:/path/to/file"
      (Left "string")

    , parseTestURI StrictPosix "file://user@hostname/path/to/file"
      (Left "endOfInput")
    ]

parseFileURIExtendedPosix :: TestTree
parseFileURIExtendedPosix =
  testGroup
    "parseFileURI (ExtendedPosix)"
    [ parseTestURI ExtendedPosix "file:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/path/to/file" })
    , parseTestURI ExtendedPosix "file:///path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/path/to/file" })
    , parseTestURI ExtendedPosix "file://hostname/path/to/file"
      (Right FileURI { fileAuth = Just "hostname", filePath = "/path/to/file" })
    , parseTestURI ExtendedPosix "file://localhost/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/path/to/file" })
    , parseTestURI ExtendedPosix "http://localhost/path/to/file"
      (Left "string")
    , parseTestURI ExtendedPosix "/path/to/file"
      (Left "string")

    , parseTestURI ExtendedPosix "file:///c:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/c:/path/to/file" })
    , parseTestURI ExtendedPosix "file:/c:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "/c:/path/to/file" })
    , parseTestURI ExtendedPosix "file:c:/path/to/file"
      (Left "string")

    , parseTestURI ExtendedPosix "file://user@hostname/path/to/file"
      (Right (FileURI {fileAuth = Just "user@hostname", filePath = "/path/to/file"}))
    ]

parseFileURIExtendedWindows :: TestTree
parseFileURIExtendedWindows =
  testGroup
    "parseFileURI (ExtendedWindows)"
    [ parseTestURI ExtendedWindows "file:///c|/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "c:/path/to/file" })
    , parseTestURI ExtendedWindows "file:/c|/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "c:/path/to/file" })
    , parseTestURI ExtendedWindows "file:c|/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "c:/path/to/file" })
    , parseTestURI ExtendedWindows "file://localhost/c|/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "c:/path/to/file" })
    , parseTestURI ExtendedWindows "file://hostname/c|/path/to/file"
      (Right FileURI { fileAuth = Just "hostname", filePath = "c:/path/to/file" })

    , parseTestURI ExtendedWindows "file://user@hostname/c|/path/to/file"
      (Right FileURI { fileAuth = Just "user@hostname", filePath = "c:/path/to/file" })

    , parseTestURI ExtendedWindows "file:///c:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "c:/path/to/file" })
    , parseTestURI ExtendedWindows "file:/c:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "c:/path/to/file" })
    , parseTestURI ExtendedWindows "file:c:/path/to/file"
      (Right FileURI { fileAuth = Nothing, filePath = "c:/path/to/file" })

    , parseTestURI ExtendedWindows "file:/path/to/file"
      (Left "Failed reading: satisfy")
    , parseTestURI ExtendedWindows "file:///path/to/file"
      (Left "Failed reading: satisfy")
    , parseTestURI ExtendedWindows "file://hostname/path/to/file"
      (Left "Failed reading: satisfy")
    , parseTestURI ExtendedWindows "file://localhost/path/to/file"
      (Left "Failed reading: satisfy")
    , parseTestURI ExtendedWindows "http://localhost/path/to/file"
      (Left "string")
    , parseTestURI ExtendedWindows "/path/to/file"
      (Left "string")
    , parseTestURI ExtendedWindows "file:///c:\\path\\to\\file"
      (Left "Failed reading: satisfy")

    , parseTestURI ExtendedWindows "file://///host.example.com/path/to/file"
      (Right (FileURI {fileAuth = Nothing, filePath = "//host.example.com/path/to/file"}))
    , parseTestURI ExtendedWindows "file:////system07/C$/"
      (Right (FileURI {fileAuth = Nothing, filePath = "//system07/C$/"}))
    ]


parseTestURI :: ParseSyntax -> ByteString -> Either String FileURI -> TestTree
parseTestURI cfg s r = testCase (B8.unpack s) $ parseFileURI cfg s @?= r


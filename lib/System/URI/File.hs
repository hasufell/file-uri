{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- |
--
-- Module      : System.URI.File
-- Description : ByteString file URI Parser
-- Copyright   : (c) Soostone Inc., 2014-2015
--                   Michael Xavier, 2014-2015
--                   Julian Ospald, 2024
-- License     : BSD3
-- Maintainer  : hasufell@posteo.de
-- Stability   : experimental
--
-- @System.URI.File@ aims to be an <https://www.rfc-editor.org/rfc/rfc8089.html RFC8089> compliant URI file parser that uses
-- efficient ByteStrings for parsing and representing the data.
--
-- As such it only parses a subset of <https://www.rfc-editor.org/rfc/rfc3986 RFC3986>, but is better at interpreting
-- the file paths. **Filepaths are always absolute according to the spec**.
--
-- Part of this module was ripped off of the <https://hackage.haskell.org/package/uri-bytestring uri-bytestring> package from
-- Soostone (specifically the host part parsing).
module System.URI.File (
  -- * Data types
  FileURI(..)
, ParseConfig(..)

  -- * Parsing
, parseFileURI

  -- * Attoparsec parsers
, fileURIStrictP
) where

import System.URI.File.Internal


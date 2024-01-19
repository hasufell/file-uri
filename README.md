# file-uri

This Haskell library parses `file:///foo/bar` URIs based on RFC 8089, including windows filepaths.
It's a subset of RFC3986, but is better at interpreting the filepaths (especially on windows).

Part of the code is based on [uri-bytestring](https://hackage.haskell.org/package/uri-bytestring)
from Soostone.


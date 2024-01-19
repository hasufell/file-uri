{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module System.URI.File.Internal where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.Bits
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Ix (range)
import Data.Word
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (decimal)

import qualified Data.ByteString as BS


-- $setup
-- >>> :set -XOverloadedStrings



    -----------------------
    --[ Main data types ]--
    -----------------------


-- | A parsed file URI. It can have an auth/host part.
data FileURI = FileURI {
    fileAuth :: Maybe ByteString   -- ^ optional host part ("localhost" is parsed as 'Nothing'); <https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats#unc-paths UNC> paths on windows go into 'filePath' and are not split
  , filePath :: ByteString         -- ^ the proper absolute filepath
  } deriving (Show, Eq)


-- | RFC syntax configuration.
data ParseSyntax = StrictPosix   -- ^ Only parses the strict syntax according to <https://www.rfc-editor.org/rfc/rfc8089.html#section-2 section 2 of RFC 8089>, which is technically posix paths.
                 | ExtendedPosix -- ^ Also parses extended user information described in <https://www.rfc-editor.org/rfc/rfc8089.html#appendix-E.1 E.1>
                 | ExtendedWindows -- ^ Parses windows paths according to <https://www.rfc-editor.org/rfc/rfc8089.html#appendix-E.1 E.1>, <https://www.rfc-editor.org/rfc/rfc8089.html#appendix-E.2 E.2> and <https://www.rfc-editor.org/rfc/rfc8089.html#appendix-E.3 E.3>. Unlike the spec, posix paths are rejected.
  deriving (Show, Eq)




    ---------------
    --[ Parsing ]--
    ---------------


-- | Parse a file URI such as @file:\/\/\/foo\/bar@ into 'FileURI'.
--
-- >>> parseFileURI StrictPosix "file:/path/to/file"
-- Right (FileURI {fileAuth = Nothing, filePath = "/path/to/file"})
-- >>> parseFileURI StrictPosix "file:///path/to/file"
-- Right (FileURI {fileAuth = Nothing, filePath = "/path/to/file"})
-- >>> parseFileURI StrictPosix "file://hostname/path/to/file"
-- Right (FileURI {fileAuth = Just "hostname", filePath = "/path/to/file"})
-- >>> parseFileURI StrictPosix "file://localhost/path/to/file"
-- Right (FileURI {fileAuth = Nothing, filePath = "/path/to/file"})
-- >>> parseFileURI StrictPosix "http://localhost/path/to/file"
-- Left "string"
-- >>> parseFileURI StrictPosix "/path/to/file"
-- Left "string"
-- >>> parseFileURI ExtendedWindows "file://///host.example.com/path/to/file"
-- Right (FileURI {fileAuth = Nothing, filePath = "//host.example.com/path/to/file"})
-- >>> parseFileURI ExtendedWindows "file:///c:/path/to/file"
-- Right (FileURI {fileAuth = Nothing, filePath = "c:/path/to/file"})
-- >>> parseFileURI ExtendedWindows "file:/c:/path/to/file"
-- Right (FileURI {fileAuth = Nothing, filePath = "c:/path/to/file"})
-- >>> parseFileURI ExtendedWindows "file:c:/path/to/file"
-- Right (FileURI {fileAuth = Nothing, filePath = "c:/path/to/file"})
parseFileURI :: ParseSyntax  -- ^ RFC syntax configuration
             -> ByteString   -- ^ input file URI
             -> Either String FileURI
parseFileURI StrictPosix     = A.parseOnly (fileURIStrictP          <* A.endOfInput)
parseFileURI ExtendedPosix   = A.parseOnly (fileURIExtendedPosixP   <* A.endOfInput)
parseFileURI ExtendedWindows = A.parseOnly (fileURIExtendedWindowsP <* A.endOfInput)



    --------------------------
    --[ Attoparsec parsers ]--
    --------------------------


-- | Parse a file URI according to the <https://www.rfc-editor.org/rfc/rfc8089.html#section-2 main ABNF in RFC 8089>, without
-- any extended rules, which is as follows:
--
-- @
--    file-URI       = file-scheme ":" file-hier-part
--
--    file-scheme    = "file"
--
--    file-hier-part = ( "//" auth-path )
--                   / local-path
--
--    auth-path      = [ file-auth ] path-absolute
--
--    local-path     = path-absolute
--
--    file-auth      = "localhost"
--                   / host
-- @
fileURIStrictP :: Parser FileURI
fileURIStrictP = A.string "file:" *> fileHierPart
 where
  fileHierPart = (A.string "//" *> authPath) <|> localPath
  authPath = (\mfA -> FileURI (if mfA == Just "localhost" then Nothing else mfA))
          <$> A.option Nothing (Just <$> fileAuth')
          <*> pathAbsoluteP
  fileAuth' = A.string "localhost" <|> hostP
  localPath = FileURI Nothing <$> pathAbsoluteP

-- | Parse a file URI according to the <https://www.rfc-editor.org/rfc/rfc8089.html#section-2 main ABNF in RFC 8089>, with
-- extended rule <https://www.rfc-editor.org/rfc/rfc8089.html#appendix-E.1 E.1>.
--
-- @
--    file-URI       = file-scheme ":" file-hier-part
--
--    file-scheme    = "file"
--
--    file-hier-part = ( "//" auth-path )
--                   / local-path
--
--    auth-path      = [ file-auth ] path-absolute
--
--    local-path     = path-absolute
--
--    file-auth      = "localhost"
--                   / [ userinfo "@" ] host
-- @
fileURIExtendedPosixP :: Parser FileURI
fileURIExtendedPosixP = A.string "file:" *> fileHierPart
 where
  fileHierPart = (A.string "//" *> authPath) <|> localPath
  authPath = (\mfA -> FileURI (if mfA == Just "localhost" then Nothing else mfA))
          <$> A.option Nothing (Just <$> fileAuth')
          <*> pathAbsoluteP
  fileAuth' = A.string "localhost" <|> sequenceM [A.option BS.empty (sequenceM [userInfoP, "@"]), hostP]
  localPath = FileURI Nothing <$> pathAbsoluteP


-- | Parse a file URI according for windows according to <https://www.rfc-editor.org/rfc/rfc8089.html#appendix-E.1 E.1>,
-- <https://www.rfc-editor.org/rfc/rfc8089.html#appendix-E.2 E.2> and
-- <https://www.rfc-editor.org/rfc/rfc8089.html#appendix-E.3 E.3>. Unlike the spec, posix paths are rejected. The ABNF
-- is a slight modification of <https://www.rfc-editor.org/rfc/rfc8089.html#appendix-F Appendix F>.
--
-- @
--    file-URI       = file-scheme ":" file-hier-part
--
--    file-scheme    = "file"
--
--    file-hier-part = ( "//" auth-path )
--                   / local-path
--
--    auth-path      = [ file-auth ] file-absolute
--                   / unc-authority path-absolute
--
--    local-path     =  drive-letter path-absolute
--                   / file-absolute
--
--    file-auth      = "localhost"
--                   / [ userinfo "@" ] host
--
--    unc-authority  = 2*3"/" file-host
--
--    file-host      = inline-IP / IPv4address / reg-name
--
--    inline-IP      = "%5B" ( IPv6address / IPvFuture ) "%5D"
--
--    file-absolute  = "/" drive-letter path-absolute
--
--    drive-letter   = ALPHA ":"
--                   / ALPHA "|"
-- @
fileURIExtendedWindowsP :: Parser FileURI
fileURIExtendedWindowsP = A.string "file:" *> fileHierPart
 where
  fileHierPart = (A.string "//" *> authPath) <|> localPath
  authPath = (\(mfA, p) -> FileURI (if mfA == Just "localhost" then Nothing else mfA) p) <$> (
          ((,) <$> A.option Nothing (Just <$> fileAuth') <*> fileAbsoluteP <* A.endOfInput)
      <|> ((,) <$> pure Nothing <*> sequenceM [uncAuthorityP, pathAbsoluteP] <* A.endOfInput)
    )
  fileAuth' = A.string "localhost" <|> sequenceM [A.option BS.empty (sequenceM [userInfoP, "@"]), hostP]
  localPath = fmap (FileURI Nothing) $ (fileAbsoluteP <* A.endOfInput) <|>
                                       (sequenceM [driveLetterP', pathAbsoluteP] <* A.endOfInput)

pathAbsoluteP :: Parser ByteString
pathAbsoluteP = sequenceM [A.string "/", A.option BS.empty $ sequenceM [segmentNZP, pathAbEmpty]]

uncAuthorityP :: Parser ByteString
uncAuthorityP = (\a b -> BS.concat [a, b]) <$> (A.string "//" <* (A.option BS.empty (A.string "/"))) <*> fileHostP

fileHostP :: Parser ByteString
fileHostP = hostP

fileAbsoluteP :: Parser ByteString
fileAbsoluteP = A.string "/" *> sequenceM [driveLetterP', pathAbsoluteP]

driveLetterP :: Parser Word8
driveLetterP = A.satisfy (A.inClass alpha) <* (A.string ":" <|> A.string "|")

-- | Like 'driveLetterP', but appends ':'.
driveLetterP' :: Parser ByteString
driveLetterP' = ((<> ":") . BS.singleton) <$> driveLetterP

userInfoP :: Parser ByteString
userInfoP = BS.pack <$> many (pctEncodedP <|> satisfyClass (":" ++ subDelims ++ unreserved))

hostP :: Parser ByteString
hostP = ipLiteralP <|> ipV4P <|> regNameP

regNameP :: Parser ByteString
regNameP = BS.pack <$> A.many1 (pctEncodedP <|> satisfyClass (subDelims ++ unreserved))

ipLiteralP :: Parser ByteString
ipLiteralP = A.word8 oBracket *> (ipVFutureP <|> ipV6P) <* A.word8 cBracket

-- | Parses IPVFuture addresses. See relevant section in RFC.
ipVFutureP :: Parser ByteString
ipVFutureP = do
  _ <- A.word8 lowercaseV
  ds <- A.takeWhile1 hexDigit
  _ <- A.word8 period
  rest <- A.takeWhile1 $ A.inClass $ subDelims ++ ":" ++ unreserved
  return $ "v" <> ds <> "." <> rest
  where
    lowercaseV = 118

-- | Parses IPV6 addresses. See relevant section in RFC.
ipV6P :: Parser ByteString
ipV6P = do
  leading <- h16s
  elided <- maybe [] (const [""]) <$> optional (A.string "::")
  trailing <- many (A.takeWhile (/= colon) <* A.word8 colon)
  (finalChunkLen, final) <- finalChunk
  let len = length (leading ++ trailing) + finalChunkLen
  when (len > 8) $ fail "Too many digits in IPv6 address"
  return $ rejoin $ [rejoin leading] ++ elided ++ trailing ++ maybeToList final
  where
    finalChunk = fromMaybe (0, Nothing) <$> optional (finalIpV4 <|> finalH16)
    finalH16 = (1,) . Just <$> h16
    finalIpV4 = (2,) . Just <$> ipV4P
    rejoin = BS.intercalate ":"
    h16s = h16 `A.sepBy` A.word8 colon
    h16 = mconcat <$> parseBetween 1 4 (A.takeWhile1 hexDigit)

-- | Parses a valid IPV4 address
ipV4P :: Parser ByteString
ipV4P =
  sequenceM
      [ decOctet,
        dot,
        decOctet,
        dot,
        decOctet,
        dot,
        decOctet
      ]
  where
    decOctet :: Parser ByteString
    decOctet = do
      (s, num) <- A.match A.decimal
      let len = BS.length s
      guard $ len <= 3
      guard $ num >= (1 :: Int) && num <= 255
      return s
    dot = A.string "."




pathAbEmpty :: Parser ByteString
pathAbEmpty = fmap BS.concat . many . sequenceM $ [A.string "/", segmentP]

segmentP :: Parser ByteString
segmentP = BS.pack <$> A.many' pcharP

segmentNZP :: Parser ByteString
segmentNZP = BS.pack <$> A.many1 pcharP

segmentNZNCP :: Parser ByteString
segmentNZNCP = BS.pack <$> A.many1 (pctEncodedP <|> satisfyClass (subDelims ++ "@" ++ unreserved))

pcharP :: Parser Word8
pcharP = pctEncodedP <|> satisfyClass (subDelims ++ ":" ++ "@" ++ unreserved)

pctEncodedP :: Parser Word8
pctEncodedP = A.string "%" *> (decode <$> A.satisfy hexDigit <*> A.satisfy hexDigit)
 where
  decode w1 w2 = combine (hexVal w1) (hexVal w2)
  hexVal w
    | 48 <= w && w <= 57  = w - 48 -- 0 - 9
    | 65 <= w && w <= 70  = w - 55 -- A - F
    | 97 <= w && w <= 102 = w - 87 -- a - f
    | otherwise           = error $ "Not a hex value: " <> show w
  combine a b = shiftL a 4 .|. b




    ---------------------------
    --[ Word/String classes ]--
    ---------------------------


hexDigit :: Word8 -> Bool
hexDigit = A.inClass "0-9a-fA-F"

unreserved :: String
unreserved = alphaNum ++ "~._-"

subDelims :: String
subDelims = "!$&'()*+,;="

alphaNum :: String
alphaNum = alpha ++ digit

alpha :: String
alpha = "a-zA-Z"

digit :: String
digit = "0-9"

oBracket :: Word8
oBracket = 91

cBracket :: Word8
cBracket = 93

colon :: Word8
colon = 58

period :: Word8
period = 46



    -------------------------------------
    --[ Custom attoparsec combinators ]--
    -------------------------------------


satisfyClass :: String -> Parser Word8
satisfyClass = A.satisfy . A.inClass

sequenceM :: Monad m => [(m ByteString)] -> m ByteString
sequenceM = fmap BS.concat . sequence

parseBetween :: (Alternative m, Monad m) => Int -> Int -> m a -> m [a]
parseBetween a b f = A.choice parsers
  where
    parsers = map (`A.count` f) $ reverse $ range (a, b)


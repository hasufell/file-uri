{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module System.URI.File (
  -- * Data types
  FileURI(..)
, ParseConfig(..)

  -- * Parsing
, parseFileURI

  -- * Attoparsec parsers
, fileURIStrictP
) where

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



    -----------------------
    --[ Main data types ]--
    -----------------------


-- | A parsed file URI. It can have an auth part.
data FileURI = FileURI {
    fileAuth :: Maybe ByteString
  , filePath :: ByteString
  } deriving (Show, Eq)


-- | RFC configuration. The strict mode follows section 2 of RFC 8089.
-- The extended mode follows the whole syntax rules in Appendix F.
data ParseConfig = Strict
                 | Extended
  deriving (Show, Eq)



    ---------------
    --[ Parsing ]--
    ---------------


parseFileURI :: ParseConfig -> ByteString -> Either String FileURI
parseFileURI Strict = A.parseOnly fileURIStrictP



    --------------------------
    --[ Attoparsec parsers ]--
    --------------------------


-- | Parse a file URI according to the main ABNF in RFC 8089, without
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
  authPath = FileURI <$> ((const Nothing <$> A.string "localhost") <|> A.option Nothing (Just <$> hostP)) <*> pathAbsoluteP
  localPath = FileURI Nothing <$> pathAbsoluteP


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

pathAbsoluteP :: Parser ByteString
pathAbsoluteP = sequenceM [A.string "/", A.option BS.empty $ sequenceM [segmentNZP, pathAbEmpty]]

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






{--
   ----------- main spec -------------

      file-URI       = file-scheme ":" file-hier-part

      file-scheme    = "file"

      file-hier-part = ( "//" auth-path )
                     / local-path

      auth-path      = [ file-auth ] path-absolute

      local-path     = path-absolute

      file-auth      = "localhost"
                     / host

   ----------- windows appendix -------------

      local-path     = [ drive-letter ] path-absolute

      drive-letter   = ALPHA ":"


   ----------- imported from RFC 3986 -------------

      host        = IP-literal / IPv4address / reg-name
      path-absolute = "/" [ segment-nz *( "/" segment ) ]

      segment       = *pchar
      segment-nz    = 1*pchar
      segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
                    ; non-zero-length segment without any colon ":"

      pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
--}


{-# LANGUAGE OverloadedStrings, TupleSections #-}
--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.Decode
-- Copyright : © 2015 Kirill Elagin
--             (c) 2006-2009, Galois, Inc.
-- License   : BSD3
--
--------------------------------------------------------------------

module Codec.MIME.Decode where

import Data.ByteString as BS
import Data.ByteString.Lazy as B
import Data.Char
import Data.Encoding (encodingFromStringExplicit, decodeStrictByteStringExplicit)
import Data.List as L
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import qualified Codec.Binary.Base64 as Base64 (decode)
import qualified Codec.Binary.QuotedPrintable as QP (decode)

-- | @decodeBody enc str@ decodes @str@ according to the scheme
-- specified by @enc@. Currently, @base64@ and @quoted-printable@ are
-- the only two encodings supported. If you supply anything else
-- for @enc@, @decodeBody@ returns @str@.
--
decodeBody :: T.Text -> B.ByteString -> B.ByteString
decodeBody enc body =
    case decode (B.toStrict body) of
      Just res -> B.fromStrict res
      Nothing  -> body
  where
    decoder =
      case T.toLower enc of
        "base64"           -> Base64.decode
        "quoted-printable" -> QP.decode
        _                  -> Right

    decode t =
      case decoder t of
        Right r -> Just r
        Left (g, b) -> if "\r\n" `BS.isPrefixOf` b
                       then fmap (g <>) $ decode (BS.drop 2 b)
                       else Nothing


decodeWords :: T.Text -> T.Text
decodeWords = T.pack . decodeWords' . T.unpack
  where
    -- Stolen from Codec.MIME.Decode
    decodeWord' :: String -> Maybe (String, String)
    decodeWord' str =
        case str of
        '=':'?':xs ->
          case dropLang $ L.break (\ch -> ch =='?' || ch == '*') xs of
            (cs,_:x:'?':bs) ->
                case toLower x of
                  'q' -> decode QP.decode cs (L.break (=='?') bs)
                  'b' -> decode Base64.decode cs (L.break (=='?') bs)
                  _   -> Nothing
            _ -> Nothing
        _ -> Nothing
      where
        -- ignore RFC 2231 extension of permitting a language tag to be supplied
        -- after the charset.
        dropLang (as,'*':bs) = (as, L.dropWhile (/='?') bs)
        dropLang (as,bs) = (as,bs)

        decode cd cset (fs,'?':'=':rs) = case cd (encodeUtf8 . T.pack $ fs) of
                                          Left _ -> Nothing
                                          Right res -> fmap (, rs) $ fromCharset cset res
        decode _ _ _ = Nothing

    decodeWords' :: String -> String
    decodeWords' "" = ""
    decodeWords' (x:xs) | isSpace x = x : decodeWords' xs
                        | otherwise =
                          case decodeWord' (x:xs) of
                            Nothing -> x : decodeWords' xs
                            Just (as,bs) -> as ++ decodeWords' bs

fromCharset :: String -> BS.ByteString -> Maybe String
fromCharset cset bs = do
  enc <- encodingFromStringExplicit cset
  case decodeStrictByteStringExplicit enc bs of
    Left _  -> Nothing
    Right r -> Just r

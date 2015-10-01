{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.Pare
-- Copyright : © 2015 Kirill Elagin
--             (c) 2006-2009, Galois, Inc.
-- License   : BSD3
--
-- Parsing MIME content.
--
--------------------------------------------------------------------
module Codec.MIME.Parse
  ( parseMIMEBody
  , parseMIMEType
  , parseMIMEMessage

  , parseHeaders
  , parseMultipart
  , parseContentType
  , splitMulti
  , normalizeCRLF
  ) where

import Codec.MIME.Type
import Codec.MIME.Decode
import Control.Arrow (second)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
import Data.Maybe
import qualified Data.List as L
import Debug.Trace (trace)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Monoid ((<>))

enableTrace :: Bool
enableTrace = True

doTrace :: String -> b -> b
doTrace | enableTrace = trace
        | otherwise   = \_ x -> x



parseMIMEBody :: [MIMEParam] -> B.ByteString -> MIMEValue
parseMIMEBody headers_in body = result { mime_val_headers = headers }
  where
  result = case mimeType mty of
    Multipart{} -> fst (parseMultipart mty body)
    Message{}   -> fst (parseMultipart mty body)
    _           -> nullMIMEValue { mime_val_type    = mty
                                 , mime_val_disp    = parseContentDisp headers
                                 , mime_val_content = Single (processBody headers body)
                                 }
  headers = [ MIMEParam (T.toLower k) v | (MIMEParam k v) <- headers_in ]
  mty = fromMaybe defaultType
                       (parseContentType =<< lookupField "content-type" (paramPairs headers))
defaultType :: Type
defaultType = Type { mimeType   = Text "plain"
                   , mimeParams = [MIMEParam "charset" "utf-8"]
                   }

parseContentDisp :: [MIMEParam] -> Maybe Disposition
parseContentDisp headers =
    (processDisp . dropFoldingWSPT) =<< lookupField "content-disposition" (paramPairs headers)
  where
    processDisp t | T.null t  = Nothing
                  | T.null bs = Just $ Disposition { dispType = toDispType (T.toLower as)
                                                   , dispParams = []
                                                   }
                  | otherwise = Just $ Disposition { dispType = toDispType (T.toLower as)
                                                   , dispParams = processParams (parseParams bs)
                                                   }
      where (as,bs) = T.break (\ch -> isSpace ch || ch == ';') t

    processParams = map procP
      where
        procP (MIMEParam as val)
            | "name" == asl              = Name val
            | "filename" == asl          = Filename val
            | "creation-date" == asl     = CreationDate val
            | "modification-date" == asl = ModDate val
            | "read-date" == asl         = ReadDate val
            | "size" == asl              = Size val
            | otherwise                  = OtherParam asl val
          where asl = T.toLower as

    toDispType t = if t == "inline" then DispInline
                   else if t == "attachment" then DispAttachment
                   else if t == "form-data"  then DispFormData
                   else  DispOther t

paramPairs :: [MIMEParam] -> [(T.Text, T.Text)]
paramPairs = map paramPair
  where
    paramPair (MIMEParam a b) = (a,b)

processBody :: [MIMEParam] -> B.ByteString -> B.ByteString
processBody headers body =
  case lookupField "content-transfer-encoding" $ paramPairs headers of
    Nothing -> body
    Just v  -> decodeBody v body

normalizeCRLF :: B.ByteString -> B.ByteString
normalizeCRLF t
    | B.null t = ""
    | "\r\n" `B.isPrefixOf` t = "\r\n" <> normalizeCRLF (B.drop 2 t)
    | any (`B.isPrefixOf` t) ["\r", "\n"] = "\r\n" <> normalizeCRLF (B.drop 1 t)
    | otherwise = let (a,b) = C.break (`elem` ['\r','\n']) t in a <> normalizeCRLF b

parseMIMEMessage :: B.ByteString -> MIMEValue
parseMIMEMessage entity =
  case parseHeaders (normalizeCRLF entity) of
   (as,bs) -> parseMIMEBody as bs

parseHeaders :: B.ByteString -> ([MIMEParam], B.ByteString)
parseHeaders str =
  case findFieldName "" str of
    Left (nm, rs) -> parseFieldValue (toTextL nm) (dropFoldingWSP rs)
    Right body    -> ([],body)
 where
  findFieldName acc t
    | B.null t = Right ""
    | "\r\n" `B.isPrefixOf` t = Right $ B.drop 2 t
    | ":" `B.isPrefixOf` t = Left (B.reverse $ C.dropWhile isHSpace acc, B.drop 1 t)
    | otherwise = findFieldName (B.take 1 t <> acc) $ B.drop 1 t

  parseFieldValue nm xs
      | B.null bs = ([MIMEParam nm (toTextL as)], "")
      | otherwise = let (zs,ys) = parseHeaders bs in (MIMEParam nm (toTextL as) :zs, ys)
    where 
      (as,bs) = takeUntilCRLF xs

parseMultipart :: Type -> B.ByteString -> (MIMEValue, B.ByteString)
parseMultipart mty body =
  case lookupField "boundary" (paramPairs $ mimeParams mty) of
    Nothing -> doTrace ("Multipart mime type, " ++ T.unpack (showType mty) ++
      ", has no required boundary parameter. Defaulting to text/plain") $
      (nullMIMEValue{ mime_val_type = defaultType
                    , mime_val_disp = Nothing
                    , mime_val_content = Single body
                    }, "")
    Just bnd -> (nullMIMEValue { mime_val_type = mty
                               , mime_val_disp = Nothing
                               , mime_val_content = Multi vals
                               }, rs)
      where (vals,rs) = splitMulti (B.fromStrict $ encodeUtf8 bnd) body

splitMulti :: B.ByteString -> B.ByteString -> ([MIMEValue], B.ByteString)
splitMulti bnd body_in =
  -- Note: we insert a CRLF if it looks as if the boundary string starts
  -- right off the bat.  No harm done if this turns out to be incorrect.
  let body | "--" `B.isPrefixOf` body_in = "\r\n" <> body_in
           | otherwise  = body_in
  in case untilMatch dashBoundary body of
       Nothing           -> mempty
       Just xs  | "--" `B.isPrefixOf` xs    -> ([], B.drop 2 xs)
                | otherwise                 -> splitMulti1 (dropTrailer xs)

 where
  dashBoundary = ("\r\n--" <> bnd)

  splitMulti1 xs 
      | B.null as && B.null bs = ([], "")
      | B.null bs = ([parseMIMEMessage as],"")
      | B.isPrefixOf "--" bs    =  ([parseMIMEMessage as], dropTrailer bs)
      | otherwise   = let (zs,ys) = splitMulti1 (dropTrailer bs)
                            in ((parseMIMEMessage as) : zs,ys)

    where
      (as,bs) = matchUntil dashBoundary xs

  dropTrailer xs 
      | "\r\n" `B.isPrefixOf` xs1 = B.drop 2 xs1
      | otherwise   = xs1 -- hmm, flag an error?
    where
       xs1 = C.dropWhile isHSpace xs 

parseMIMEType :: T.Text -> Maybe Type
parseMIMEType = parseContentType

parseContentType :: T.Text -> Maybe Type
parseContentType str
    | T.null minor0 = doTrace ("unable to parse content-type: " ++ show str) $ Nothing
    | otherwise     = Just Type  { mimeType = toType maj as
                                 , mimeParams = parseParams (T.dropWhile isHSpace bs)
                                 }
  where
    (maj, minor0) = T.break (=='/') (dropFoldingWSPT str)
    minor = T.drop 1 minor0
    (as, bs) = T.break (\ ch -> isHSpace ch || isTSpecial ch) minor
    toType a b = case lookupField (T.toLower a) mediaTypes of
         Just ctor -> ctor b
         _ -> Other a b

parseParams :: T.Text -> [MIMEParam]
parseParams t   | T.null t          = []
                | ';' == T.head t   = let (nm_raw, vs0) = T.break (=='=') (dropFoldingWSPT $ T.tail t)
                                          nm = T.toLower nm_raw in
                    if T.null vs0
                        then []
                        else let vs = T.tail vs0 in
                            if not (T.null vs) && T.head vs == '"'
                                then let vs1 = T.tail vs
                                         (val, zs0) = T.break (=='"') vs1 in
                                    if T.null zs0
                                        then [MIMEParam nm val]
                                        else MIMEParam nm val : parseParams (T.dropWhile isHSpace $ T.tail zs0)
                                else let (val, zs) = T.break (\ch -> isHSpace ch || isTSpecial ch) vs in
                                    MIMEParam nm val : parseParams (T.dropWhile isHSpace zs)
                | otherwise = doTrace ("Codec.MIME.Parse.parseParams: curious param value -- " ++ show t) []

mediaTypes :: [(T.Text, T.Text -> MIMEType)]
mediaTypes =
  [ ("multipart",   (Multipart . toMultipart))
  , ("application", Application)
  , ("audio",       Audio)
  , ("image",       Image)
  , ("message",     Message)
  , ("model",       Model)
  , ("text",        Text)
  , ("video",       Video)
  ]
 where toMultipart b = fromMaybe other (lookupField (T.toLower b) multipartTypes)
          where other | T.isPrefixOf "x-" b = Extension b
                      | otherwise           = OtherMulti b

multipartTypes :: [(T.Text, Multipart)]
multipartTypes =
  [ ("alternative", Alternative)
  , ("byteranges",  Byteranges)
  , ("digest",      Digest)
  , ("encrypted",   Encrypted)
  , ("form-data",   FormData)
  , ("mixed",       Mixed)
  , ("parallel",    Parallel)
  , ("related",     Related)
  , ("signed",      Signed)
  ]

untilMatch :: B.ByteString -> B.ByteString -> Maybe B.ByteString
untilMatch a b  | B.null a  = Just b
                | B.null b  = Nothing
                | a `B.isPrefixOf` b = Just $ B.drop (B.length a) b
                | otherwise = untilMatch a $ B.tail b

{-
matchUntil :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
-- searching str; returning parts before str and after str
matchUntil str = second (B.drop $ B.length str) . B.breakOn str
-}

matchUntil :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
matchUntil _   "" = ("", "")
matchUntil str xs
    | B.null xs = mempty
    -- slow, but it'll do for now.
    | str `B.isPrefixOf` xs = ("", B.drop (B.length str) xs)
    | otherwise = let (as,bs) = matchUntil str $ B.tail xs in (B.take 1 xs <> as, bs)

isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

isTSpecial :: Char -> Bool
isTSpecial x = x `elem` ("()<>@,;:\\\"/[]?="::String) -- "

dropFoldingWSP :: B.ByteString -> B.ByteString
dropFoldingWSP t | B.null t   = ""
                 | isHSpace (C.head t) = dropFoldingWSP $ B.tail t
                 | "\r\n" `B.isPrefixOf` t && not (B.null $ B.drop 2 t) && isHSpace (C.head $ B.drop 2 t)
                    = dropFoldingWSP $ B.drop 3 t
                 | otherwise    = t

dropFoldingWSPT :: T.Text -> T.Text
dropFoldingWSPT = toText . B.toStrict . dropFoldingWSP . B.fromStrict . encodeUtf8

takeUntilCRLF :: B.ByteString -> (B.ByteString, B.ByteString)
takeUntilCRLF str = go "" str
 where
  go acc t  | B.null t  = (B.reverse (C.dropWhile isHSpace acc), "")
            | "\r\n" `B.isPrefixOf` t && not (B.null $ B.drop 2 t) && isHSpace (C.head $ B.drop 2 t)
                        = go (" " <> acc) (B.drop 3 t)
            | "\r\n" `B.isPrefixOf` t && not (B.null $ B.drop 2 t)
                        = (B.reverse (C.dropWhile isHSpace acc), B.drop 2 t)
            | otherwise = go (B.take 1 t <> acc) $ B.tail t

-- case in-sensitive lookup of field names or attributes\/parameters.
lookupField :: T.Text -> [(T.Text,a)] -> Maybe a
lookupField n ns =
   -- assume that inputs have been mostly normalized already
   -- (i.e., lower-cased), but should the lookup fail fall back
   -- to a second try where we do normalize before giving up.
  case lookup n ns of
    x@Just{} -> x
    Nothing  ->
      let nl = T.toLower n in
      fmap snd $ L.find ((nl==) . T.toLower . fst) ns

toText :: BS.ByteString -> T.Text
toText = decodeUtf8With lenientDecode

toTextL :: B.ByteString -> T.Text
toTextL = decodeUtf8With lenientDecode . B.toStrict

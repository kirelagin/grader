{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import Codec.Binary.Base64 as Base64 (decode)
import Codec.Binary.QuotedPrintable as QP (decode)
import Codec.MIME.Parse (parseMIMEMessage)
import Codec.MIME.Type
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans.Maybe (maybeToExceptT)
import Control.Monad.State
import Data.ByteString as BS
import Data.ByteString.Char8 as C8
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as LC8
import Data.Char as C
import Data.Encoding
import Data.List as L
import Data.Map as M
import Data.Maybe (listToMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.ICU as TICU (regex, find, group)
import Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath.Posix ((</>), takeFileName, dropExtension)
import System.IO (openTempFile, hClose)
import Text.Email.Validate (EmailAddress, emailAddress)

import Grader.Course
import Grader.Monad
import Grader.Submission


data ReceiveMailError = InitError GraderInitError
                      | InvalidFrom T.Text
                      | CourseNotFound T.Text
  deriving Show

main :: IO()
main = do
    args <- fmap (L.map T.pack) getArgs
    case args of
      course:from:[] -> do
        result <- runExceptT . evalGrader' defaultConf InitError $
                    receiveMail course from
        case result of
          Right () -> return ()
          Left e -> die (show e)
      _ -> die $ "2 args expected, got " ++ show (L.length args)


receiveMail :: T.Text -> T.Text -> Grader ReceiveMailError ()
receiveMail to from = do
    cdb <- gets courses
    adb <- gets aliases
    rawPath <- asks ((</> "raw-mail") . baseDir)
    (rawId, message) <- liftIO $ saveRaw to rawPath
    case emailAddress $ encodeUtf8 from of
      Nothing -> throwError $ InvalidFrom from
      Just email -> case findCourse cdb adb to email of
                      Nothing -> throwError $ CourseNotFound to
                      Just course@(Course cn) -> do
                        (asg, authorName, msg, files) <- processMessage message
                        liftIO . TIO.putStr $ " for " <> cn <> "/" <> asg <> " by '" <> authorName <> "'"
                        let msg' = "RAW-ID: " <> rawId <> "\n\n" <> msg
                        cid <- withCourseRepo course $ buildTree files >>= addSubmission (Assignment asg course) email authorName msg'
                        liftIO . TIO.putStrLn $ " at " <> (T.pack . show) cid
                        return ()
  where
    saveRaw :: T.Text -> FilePath -> IO (Text, Text)
    saveRaw course dir = do
      createDirectoryIfMissing True dir
      (n, h) <- openTempFile dir (T.unpack course ++ "-")
      mbs <- BL.getContents
      BL.hPutStr h mbs
      hClose h
      let fileName = takeFileName n
      Prelude.putStr $ "Saved " <> fileName
      return $ (T.pack fileName, decodeUtf8With lenientDecode . BL.toStrict $ mbs)

    processMessage :: Text -> Grader ReceiveMailError (T.Text, T.Text, T.Text, Map T.Text BL.ByteString)
    processMessage text = do
        let parsed@(MIMEValue _ _ _ headers _) = parseMIMEMessage text
        let authorName = maybe "" id $ extractName =<< findParam "From" headers
        let text = maybe "" id $ extractText parsed
        let attachments = extractAttachments parsed
        let assignment = case M.keys attachments of
                           [fname] -> T.pack . dropExtension . T.unpack $ fname
                           _ -> case findParam "Subject" headers of
                                  Just subj -> subj
                                  Nothing   -> ""
        return (T.toLower assignment, authorName, text, attachments)

findParam :: Text -> [MIMEParam] -> Maybe Text
findParam field params = fmap decodeWords $ findParamRaw (T.toLower field) params
  where
    findParamRaw :: Text -> [MIMEParam] -> Maybe BS.ByteString
    findParamRaw field params = fmap encodeUtf8 $ L.lookup field $ L.map (\(MIMEParam n v) -> (n, v)) params

    ------------------------

    decodeWords :: BS.ByteString -> Text
    decodeWords = T.pack . decodeWords' . T.unpack . decodeUtf8With lenientDecode

    -- Stolen from Codec.MIME.Decode
    decodeWord' :: String -> Maybe (String, String)
    decodeWord' str =
        case str of
        '=':'?':xs ->
          case dropLang $ L.break (\ch -> ch =='?' || ch == '*') xs of
            (cs,_:x:'?':bs) ->
                case C.toLower x of
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
    Left _ -> Nothing
    Right r -> Just r

extractAttachments :: MIMEValue -> M.Map T.Text BL.ByteString
extractAttachments val = execState (extract val) M.empty
  where
    extract :: MIMEValue -> State (M.Map T.Text BL.ByteString) ()
    extract (MIMEValue (Type _ tparams) mdisp cont _ _) =
      case cont of
        Multi vals -> mapM_ extract vals
        Single t   -> case mdisp of
                        Just (Disposition DispAttachment params) -> case getDispFilename params of
                                                                      Just "signature.asc" -> return ()
                                                                      Just fn -> modify $ M.insert fn (LC8.pack . T.unpack $ t)
                                                                      _ -> return ()
                        _ -> return ()
    getDispFilename (Filename t : _) = Just t
    getDispFilename (x : rest)       = getDispFilename rest
    getDispFilename []               = Nothing


extractText :: MIMEValue -> Maybe T.Text
extractText val = listToMaybe . join . fmap maybeToList $ [extract isTextPlain, extract isTextHtml, extract isTextAny] <*> pure val
  where
    extract :: (Type -> Bool) -> MIMEValue -> Maybe T.Text
    extract pred (MIMEValue tp@(Type _ tparams) mdisp cont _ _) =
      case cont of
        Multi vals -> listToMaybe . join . fmap maybeToList $ L.map (extract pred) vals
        Single   t -> let dispOk = case mdisp of
                                     Nothing -> True
                                     Just (Disposition DispInline _) -> True
                                     _ -> False
                      in if dispOk && pred tp then Just (tryDecodeText tparams t) else Nothing

    isTextPlain :: Type -> Bool
    isTextPlain (Type (Text "plain") _) = True
    isTextPlain _ = False

    isTextHtml :: Type -> Bool
    isTextHtml (Type (Text "html") _) = True
    isTextHtml _ = False

    isTextAny :: Type -> Bool
    isTextAny (Type (Text _) _) = True
    isTextAny _ = False

    tryDecodeText :: [MIMEParam] -> T.Text -> T.Text
    tryDecodeText (MIMEParam "charset" charset : _) = tryDecode (T.unpack charset) . C8.pack . T.unpack
      where tryDecode cset bs = case fromCharset cset bs of
                                  Just t  -> T.pack t
                                  Nothing -> decodeUtf8With lenientDecode $ bs
    tryDecodeText (_ : rest) = tryDecodeText rest
    tryDecodeText [] = id

extractName :: T.Text -> Maybe T.Text
extractName from = TICU.find (TICU.regex [] "^(?:^|(.*?)\\s+)<\\S+>$") from >>= TICU.group 1

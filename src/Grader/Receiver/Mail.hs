{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import Codec.MIME.Decode (fromCharset, decodeWords)
import Codec.MIME.Parse (parseMIMEMessage)
import Codec.MIME.Type
import Control.Monad (msum)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Char as C
import Data.List as L
import Data.Map as M
import Data.Maybe (fromMaybe)
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
import System.Posix.Files (setFileCreationMask, otherModes)
import Text.Email.Validate (emailAddress)

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
        umaskOld <- setFileCreationMask otherModes
        result <- runExceptT . evalGrader' defaultConf InitError $
                    receiveMail course from
        _ <- setFileCreationMask umaskOld
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
                        (msgId, asg, authorName, msg, files) <- processMessage message
                        liftIO . TIO.putStr $ " for " <> cn <> "/" <> asg
                        let msg' = "RAW-ID: " <> rawId <> "\nMessage-ID: " <> msgId <> "\n==\n\n" <> msg
                        cid <- withCourseRepo course $ buildTree files >>= addSubmission (Assignment asg course) email authorName msg'
                        liftIO . TIO.putStrLn $ " at " <> (T.pack . show) cid
                        return ()
  where
    saveRaw :: T.Text -> FilePath -> IO (Text, BL.ByteString)
    saveRaw course dir = do
      createDirectoryIfMissing True dir
      (n, h) <- openTempFile dir (T.unpack course ++ "-")
      mbs <- BL.getContents
      BL.hPutStr h mbs
      hClose h
      let fileName = takeFileName n
      Prelude.putStr $ "Saved " <> fileName
      return $ (T.pack fileName, mbs)

    processMessage :: BL.ByteString -> Grader ReceiveMailError (T.Text, T.Text, T.Text, T.Text, Map T.Text BL.ByteString)
    processMessage text = do
        let parsed@(MIMEValue _ _ _ headers _) = parseMIMEMessage text
        let msgId = fromMaybe "" $ findParam "Message-ID" headers
        let authorName = fromMaybe "" $ extractName =<< findParam "From" headers
        let text = fromMaybe "" $ extractText parsed
        let attachments = extractAttachments parsed
        let assignment = case M.keys attachments of
                           [fname] -> T.pack . dropExtension . T.unpack $ fname
                           _ -> case findParam "Subject" headers of
                                  Just subj -> subj
                                  Nothing   -> ""
        return (msgId, T.toLower assignment, authorName, text, attachments)

findParam :: Text -> [MIMEParam] -> Maybe Text
findParam field params = fmap decodeWords $ L.lookup (T.toLower field) $ L.map (\(MIMEParam n v) -> (n, v)) params


extractAttachments :: MIMEValue -> M.Map T.Text BL.ByteString
extractAttachments val = execState (extract val) M.empty
  where
    extract :: MIMEValue -> State (M.Map T.Text BL.ByteString) ()
    extract (MIMEValue (Type _ tparams) mdisp cont _ _) =
      case cont of
        Multi vals -> mapM_ extract vals
        Single t   -> case mdisp of
                        Just (Disposition DispAttachment params) -> case getDispFilename params tparams of
                                                                      Just "signature.asc" -> return ()
                                                                      Just fn -> modify $ M.insert fn t
                                                                      _ -> return ()
                        _ -> return ()
    getDispFilename (Filename t : _) _ = Just (decodeWords t)
    getDispFilename (_ : rest) tparams = getDispFilename rest tparams
    getDispFilename [] tparams         = getParamFilename tparams

    getParamFilename = findParam "name"


extractText :: MIMEValue -> Maybe T.Text
extractText val = msum $ [extract isTextPlain, extract isTextHtml, extract isTextAny] <*> pure val
  where
    extract :: (Type -> Bool) -> MIMEValue -> Maybe T.Text
    extract pred (MIMEValue tp@(Type _ tparams) mdisp cont _ _) =
      case cont of
        Multi vals -> msum $ L.map (extract pred) vals
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

    tryDecodeText :: [MIMEParam] -> BL.ByteString -> T.Text
    tryDecodeText (MIMEParam "charset" charset : _) = tryDecode (T.unpack charset) . BL.toStrict
      where tryDecode cset bs = case fromCharset cset bs of
                                  Just t  -> T.pack t
                                  Nothing -> decodeUtf8With lenientDecode $ bs
    tryDecodeText (_ : rest) = tryDecodeText rest
    tryDecodeText [] = decodeUtf8With lenientDecode . BL.toStrict

extractName :: T.Text -> Maybe T.Text
extractName from = TICU.find (TICU.regex [] "^(?:^|(.*?)\\s+)<\\S+>$") from >>= TICU.group 1

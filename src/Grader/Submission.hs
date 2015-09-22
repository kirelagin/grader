{-# LANGUAGE OverloadedStrings #-}

module Grader.Submission
  ( buildTree, addSubmission
  , CommitOid, TreeOid
  )
  where

import Control.Monad.Except
import Control.Monad.Logger (NoLoggingT(..))
import Data.ByteString.Lazy as BL
import Data.Map as M
import Data.Maybe (maybeToList)
import Data.Text as T (Text, intercalate)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.LocalTime (getZonedTime)
import Git hiding (CommitOid, TreeOid)
import Git.Libgit2

import Grader.Course
import Grader.Monad
import Grader.User


liftGit :: ReaderT LgRepo (NoLoggingT IO) a -> Grader a
liftGit = Grader . lift . lift . lift


buildTree :: Map Text ByteString -> Grader TreeOid
buildTree files = liftGit $ do
  files' <- mapM (createBlob . BlobStringLazy) files
  createTree $ forM_ (M.toList files') $ \(path, bOid) -> putBlob (encodeUtf8 path) bOid

addSubmission :: Assignment -> EmailAddress -> TreeOid -> Text -> Grader CommitOid
addSubmission (Assignment an (Course cn)) email tOid msg = do
  parentOid <- liftGit $ fmap maybeToList $ lookupParent
  now <- liftIO $ getZonedTime
  authorName <- gets (getUserName email . users)
  let sig = Signature authorName (emailToText email) now
  commit <- liftGit $ createCommit parentOid tOid sig sig msg (Just submissionRef)
  return $ commitOid commit

  where
    submissionRef = T.intercalate "/" ["refs", "submissions", cn, an, emailToText email]

    lookupParent = do
      mparentOid <- resolveReference submissionRef
      case mparentOid of
        Just oid -> do
          obj <- lookupObject oid
          case obj of
            CommitObj c -> return (Just $ commitOid c)
            _           -> return Nothing
        Nothing  -> return Nothing

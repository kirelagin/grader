{-# LANGUAGE OverloadedStrings #-}

module Grader.Submission
  ( GraderGit, withCourseRepo
  , buildTree, addSubmission
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
import Grader.Paths
import Grader.User (EmailAddress, emailToText)


type GraderGit a = ReaderT LgRepo (NoLoggingT IO) a

--liftGit :: GraderGit a -> Grader a
--liftGit = Grader . lift . lift . lift

withCourseRepo :: Course -> GraderGit a -> Grader a
withCourseRepo (Course c) g = do
  courseRepoPath <- asks (courseDir c . baseDir)
  let repoOptions = RepositoryOptions courseRepoPath Nothing False True
  liftIO $ withRepository' lgFactory repoOptions g


buildTree :: Map Text ByteString -> GraderGit TreeOid
buildTree files = do
  files' <- mapM (createBlob . BlobStringLazy) files
  createTree $ forM_ (M.toList files') $ \(path, bOid) -> putBlob (encodeUtf8 path) bOid

addSubmission :: Assignment -> EmailAddress -> Text -> TreeOid -> Text -> GraderGit CommitOid
addSubmission (Assignment an _) email authorName tOid msg = do
  parentOid <- fmap maybeToList $ lookupParent
  now <- liftIO $ getZonedTime
  let sig = Signature authorName (emailToText email) now
  commit <- createCommit parentOid tOid sig sig msg (Just submissionRef)
  return $ commitOid commit

  where
    submissionRef = T.intercalate "/" ["refs", "submissions", an, emailToText email]

    lookupParent = do
      mparentOid <- resolveReference submissionRef
      case mparentOid of
        Just oid -> do
          obj <- lookupObject oid
          case obj of
            CommitObj c -> return (Just $ commitOid c)
            _           -> return Nothing
        Nothing  -> return Nothing

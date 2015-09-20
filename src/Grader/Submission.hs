{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Grader.Submission
  ( SubmissionsRepo, openRepo
  , buildTree, addSubmission
  , execGit
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
import Git
import Git.Libgit2 (lgFactory, LgRepo)

import Grader.Course
import Grader.Monad
import Grader.Paths
import Grader.User


type SubmissionsRepo = FilePath

data RepoOpenError = RepoOpenError IOError
  deriving Show

openRepo :: FilePath -> ExceptT RepoOpenError IO SubmissionsRepo
openRepo = return


type GitGrader b = ReaderT LgRepo (NoLoggingT Grader) b


buildTree :: Map Text ByteString -> GitGrader (TreeOid LgRepo)
buildTree files = do
  files' <- mapM (createBlob . BlobStringLazy) files
  createTree $ forM_ (M.toList files') $ \(path, bOid) -> putBlob (encodeUtf8 path) bOid

addSubmission :: Assignment -> EmailAddress -> TreeOid LgRepo -> Text -> GitGrader (CommitOid LgRepo)
addSubmission (Assignment an (Course cn)) email tOid msg = do
  parentOid <- fmap maybeToList $ lookupParent
  now <- liftIO $ getZonedTime
  authorName <- gets (getUserName email . users)
  let sig = Signature authorName (emailToText email) now
  commit <- createCommit parentOid tOid sig sig msg (Just submissionRef)
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

execGit :: GitGrader b -> Grader b
execGit g = do
  repoPath <- asks $ repoDir . baseDir
  let repoOptions = RepositoryOptions repoPath Nothing True True
  withRepository' lgFactory repoOptions g

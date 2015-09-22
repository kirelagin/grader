{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grader.Monad
  ( module Control.Monad.Reader
  , module Control.Monad.State
  , GraderConf(..), defaultConf
  , GraderState(..)
  , GraderError(..)
  , Grader(..), runGrader, evalGrader, evalGrader'
  , initGrader
  )
  where

import Control.Monad.Trans.Except
import Control.Monad.Logger (NoLoggingT(..))
import Control.Monad.Reader
import Control.Monad.State
import Data.Yaml
import Git (RepositoryOptions(..), withRepository')
import Git.Libgit2 (LgRepo, lgFactory)

import Grader.Course
import Grader.Paths
import Grader.User


data GraderConf = GraderConf
  { baseDir :: FilePath
  }

defaultConf :: GraderConf
defaultConf = GraderConf "."

data GraderState = GraderState
  { users   :: UserDB
  , courses :: CourseDB
  , aliases :: AliasDB
  }

data GraderError = UsersParseError ParseException
                 | CoursesLoadError CoursesLoadError
  deriving Show

newtype Grader a = Grader (ReaderT GraderConf (StateT GraderState (ExceptT GraderError (ReaderT LgRepo (NoLoggingT IO)))) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState GraderState, MonadReader GraderConf)


runGrader :: GraderConf -> GraderState -> Grader a -> ExceptT GraderError IO (a, GraderState)
runGrader cnf st (Grader a) = ExceptT $ withRepository' lgFactory repoOptions $ runExceptT (runStateT (runReaderT a cnf) st)
  where
    repoOptions = RepositoryOptions repositoryPath Nothing True True
    repositoryPath = repoDir . baseDir $ cnf

evalGrader :: GraderConf -> GraderState -> Grader a -> ExceptT GraderError IO a
evalGrader cnf st g = fmap fst $ runGrader cnf st g

evalGrader' :: GraderConf -> Grader a -> ExceptT GraderError IO a
evalGrader' cnf g = do
  st <- initGrader cnf
  evalGrader cnf st g


initGrader :: GraderConf -> ExceptT GraderError IO GraderState
initGrader cnf = do
    users <- withExceptT UsersParseError $
              loadUsers (userDB . baseDir $ cnf)
    (courses, aliases) <- withExceptT CoursesLoadError $
              loadCourses (coursesDir . baseDir $ cnf)
    return $ GraderState users courses aliases

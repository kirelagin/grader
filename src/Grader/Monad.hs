{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grader.Monad
  ( module Control.Monad.Reader
  , module Control.Monad.State
  , GraderConf(..), defaultConf
  , GraderState(..)
  , GraderInitError(..), NoError
  , Grader(..), runGrader, evalGrader, evalGrader', evalGrader''
  , initGrader
  )
  where

import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Except
import Control.Monad.Logger (NoLoggingT(..))
import Control.Monad.Reader
import Control.Monad.State
import Data.Yaml (ParseException)
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

newtype Grader e a = Grader (ReaderT GraderConf (StateT GraderState (ExceptT e (ReaderT LgRepo (NoLoggingT IO)))) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState GraderState, MonadReader GraderConf)


runGrader :: GraderConf -> GraderState -> Grader e a -> ExceptT e IO (a, GraderState)
runGrader cnf st (Grader a) = ExceptT $ withRepository' lgFactory repoOptions $ runExceptT (runStateT (runReaderT a cnf) st)
  where
    repoOptions = RepositoryOptions repositoryPath Nothing True True
    repositoryPath = repoDir . baseDir $ cnf

evalGrader :: GraderConf -> GraderState -> Grader e a -> ExceptT e IO a
evalGrader cnf st g = fmap fst $ runGrader cnf st g

evalGrader' :: GraderConf -> (GraderInitError -> e) -> Grader e a -> ExceptT e IO a
evalGrader' cnf initError g = do
  st <- withExceptT initError $ initGrader cnf
  evalGrader cnf st g

data NoError

evalGrader'' :: GraderConf -> Grader NoError a -> ExceptT GraderInitError IO a
evalGrader'' cnf g = do
    st <- initGrader cnf
    withExceptT noError $ evalGrader cnf st g
  where
    noError _ = undefined


data GraderInitError = UsersParseError ParseException
                     | CoursesLoadError CoursesLoadError
  deriving Show

initGrader :: GraderConf -> ExceptT GraderInitError IO GraderState
initGrader cnf = do
    users <- withExceptT UsersParseError $
              loadUsers (userDB . baseDir $ cnf)
    (courses, aliases) <- withExceptT CoursesLoadError $
              loadCourses (coursesDir . baseDir $ cnf)
    return $ GraderState users courses aliases

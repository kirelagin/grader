{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Grader.Monad
  ( module Control.Monad.Reader
  , module Control.Monad.State
  , GraderConf(..), defaultConf
  , GraderState(..)
  , GraderError(..)
  , Grader, runGrader, evalGrader, evalGrader'
  , initGrader
  )
  where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask(..), MonadThrow)
--import Control.Monad.Except
import Control.Monad.Trans.Except
--import Control.Monad.Logger (MonadLogger, NoLoggingT(..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Yaml
--import Bindings.Libgit2.Types
--import Foreign.ForeignPtr
--import Git
--import Git.Libgit2
--import Git.Libgit2.Types (MonadExcept)

import Grader.Course
import Grader.Paths
import Grader.User


data GraderConf = GraderConf
  { baseDir :: FilePath
--  , repo    :: LgRepo
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

newtype Grader a = Grader { unGrader :: ReaderT GraderConf (StateT GraderState (ExceptT GraderError IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState GraderState, MonadReader GraderConf, MonadBase IO, MonadCatch, MonadMask, MonadThrow)


runGrader :: GraderConf -> GraderState -> Grader a -> ExceptT GraderError IO (a, GraderState)
runGrader cnf st (Grader a) = runStateT (runReaderT a cnf) st

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


----------------
-- Shit for Git
----------------

instance MonadBaseControl IO Grader where
  type StM Grader a = StM (ReaderT GraderConf (StateT GraderState (ExceptT GraderError IO))) a
  liftBaseWith f = Grader . liftBaseWith $ \runInBase -> f $ runInBase . unGrader
  restoreM = Grader . restoreM

instance MonadMask m => MonadMask (ExceptT e m) where
  mask h = ExceptT $ mask $ \u -> runExceptT (h $ q u)
    where q :: (m (Either e a) -> m (Either e a)) -> ExceptT e m a -> ExceptT e m a
          q u (ExceptT b) = ExceptT (u b)

  uninterruptibleMask h = ExceptT $ uninterruptibleMask $ \u -> runExceptT (h $ q u)
    where q :: (m (Either e a) -> m (Either e a)) -> ExceptT e m a -> ExceptT e m a
          q u (ExceptT b) = ExceptT (u b)

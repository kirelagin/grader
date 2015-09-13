{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Grader.Course
  ( AliasDB, CourseDB
  , Course(..)
  , loadCourses, CoursesLoadError(..)
  )
  where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Extra
import Data.Map as M (Map, fromList)
import Data.Text as T (Text, pack)
import Data.Yaml as Y
import GHC.Generics
import System.Directory
import System.FilePath.Posix ((</>))
import System.Posix.Files (getFileStatus, isDirectory)


type CourseDB = Map Text Course
type AliasDB  = Map Text Text

data Course = Course
  { name :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)


data CoursesLoadError = LoadingIOError IOError
  deriving Show

loadCourses :: FilePath -> ExceptT CoursesLoadError IO (CourseDB, AliasDB)
loadCourses path = do
    names' <- (withExceptT LoadingIOError . ExceptT . try) $
              getDirectoryContents path
    (ds, fs) <- (withExceptT LoadingIOError . ExceptT . try) $
                partitionM (fmap isDirectory . getFileStatus . (path </>)) $ filter (\n -> n /= "." && n /= "..") names'
    return $ (fromList . map ((\n -> (n, Course n)) . pack) $ ds, fromList . map ((\n -> (n, n)) . pack) $ fs)

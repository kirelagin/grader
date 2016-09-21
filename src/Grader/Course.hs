{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TupleSections #-}

module Grader.Course
  ( AliasDB, CourseDB
  , Course(..), Alias(..), Assignment(..)
  , loadCourses, CoursesLoadError(..)
  , getCourse, findCourse
  )
  where

import Control.Exception
import Control.Monad.Except
import Data.List as L (partition)
import Data.Map as M (Map, fromList, lookup)
import Data.Text as T (Text, pack)
import Data.Yaml as Y
import GHC.Generics
import System.Directory (getDirectoryContents)
import System.FilePath.Posix ((</>), takeBaseName)
import System.Posix.Files (FileStatus, getSymbolicLinkStatus
                          , isDirectory, isSymbolicLink
                          , readSymbolicLink
                          )

import Grader.User


type CourseDB = Map Text Course
type AliasDB  = Map Text Alias

data Course = Course Text
  deriving (Show, Generic, ToJSON, FromJSON)

data Alias = Unconditional Text | Conditional (EmailAddress -> Maybe Text)


data Assignment = Assignment Text Course


data CoursesLoadError = LoadingIOError IOError
  deriving Show

loadCourses :: FilePath -> ExceptT CoursesLoadError IO (CourseDB, AliasDB)
loadCourses path = do
    names    <- fmap (filter (\n -> n /= "." && n /= "..")) $ tryLoad (getDirectoryContents path)
    stnames  <- tryLoad $ mapM (\n -> fmap (, n) $ getSymbolicLinkStatus (path </> n)) names
    let (as, cs) = partition isAlias stnames

    courses <- tryLoad $ loadEach loadCourse cs
    aliases <- tryLoad $ loadEach loadAlias as
    return (courses, aliases)

  where
    tryLoad = withExceptT LoadingIOError . ExceptT . try

    isAlias :: (FileStatus, FilePath) -> Bool
    isAlias (s, _) = isSymbolicLink s || (not . isDirectory) s

    loadEach :: (FileStatus -> FilePath -> IO a) -> [(FileStatus, FilePath)] -> IO (Map Text a)
    loadEach f = fmap M.fromList . mapM (\(s, p) -> fmap (pack p,) (f s p))

    loadCourse :: FileStatus -> FilePath -> IO Course
    loadCourse _ = return . Course . pack

    loadAlias :: FileStatus -> FilePath -> IO Alias
    loadAlias s p = if (isSymbolicLink s)
                    then do
                      target <- readSymbolicLink (path </> p)
                      -- TODO: check that the link does not point outside
                      return $ Unconditional $ pack (takeBaseName target)
                    else return $ Conditional (\_ -> Nothing)  -- TODO


getCourse :: CourseDB -> Text -> Maybe Course
getCourse = flip M.lookup

findCourse :: CourseDB -> AliasDB -> Text -> EmailAddress -> Maybe Course
findCourse cs as t u = case getCourse cs t of
                         Just c  -> return c
                         Nothing -> do
                                      alias <- M.lookup t as
                                      case alias of
                                        Unconditional name -> getCourse cs name
                                        Conditional f -> f u >>= getCourse cs

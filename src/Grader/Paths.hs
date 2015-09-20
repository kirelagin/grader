{-# LANGUAGE FlexibleContexts #-}

module Grader.Paths where

import Data.Text
import System.FilePath.Posix ((</>))


userDB :: FilePath -> FilePath
userDB = (</> "users.yaml")

coursesDir :: FilePath -> FilePath
coursesDir = (</> "courses")

aliasesDir :: FilePath -> FilePath
aliasesDir = (</> "aliases")

courseDir :: Text -> FilePath -> FilePath
courseDir course = (</> unpack course) . coursesDir

repoDir :: FilePath -> FilePath
repoDir = (</> "repo")

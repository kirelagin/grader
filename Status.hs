{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Data.Map as M (keys)
import Data.Text as T (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import System.Exit (die)
import Text.Email.Validate (toByteString)

import Grader


getStatus :: Grader ([Text], [Text], [Text])
getStatus = do
  us <- gets (keys . users)
  cs <- gets (keys . courses)
  as <- gets (keys . aliases)
  return (map (decodeUtf8 . toByteString) us, cs, as)

main :: IO ()
main = do
    let conf = defaultConf { baseDir = "/tmp/grader" }
    result <- runExceptT $ evalGrader' conf getStatus
    case result of
      Left e -> die (show e)
      Right (us, cs, as) -> do
        printSection "Users" us
        putStrLn ""
        printSection "Courses" cs
        putStrLn ""
        printSection "Course aliases" as
  where
    printSection :: Text -> [Text] -> IO ()
    printSection title vals = do
      putStrLn $ unpack title ++ " (" ++ show (length vals) ++ "):"
      mapM_ (\v -> putStrLn $ "- " ++ unpack v) vals

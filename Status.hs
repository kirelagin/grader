{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Data.Map as M (keys)
import Data.Text as T (Text, unpack)
import System.Exit (die)

import Grader


getStatus :: Grader NoError ([Text], [Text], [Text])
getStatus = do
  us <- gets (keys . users)
  cs <- gets (keys . courses)
  as <- gets (keys . aliases)
  return (map emailToText us, cs, as)

main :: IO ()
main = do
    result <- runExceptT $ evalGrader'' defaultConf getStatus
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

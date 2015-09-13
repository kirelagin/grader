{-# LANGUAGE FlexibleInstances #-}

module Grader.User
  ( FirstName, LastName, UserDB
  , addUser
  , loadUsers
  )
  where

import Control.Monad.Except
import Data.Map as M
import Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Yaml as Y
import Text.Email.Validate (EmailAddress, emailAddress, toByteString)


type FirstName = Text
type LastName = Text
type UserDB = Map EmailAddress (FirstName, LastName)

data UserCreationError = InvalidEmail Text
  deriving Show

instance ToJSON (Map EmailAddress (FirstName, LastName)) where
  toJSON = toJSON . mapKeys (decodeUtf8 . toByteString)

instance FromJSON (Map EmailAddress (FirstName, LastName)) where
  parseJSON o = parseJSON o >>= \m -> case runExcept $ checkEmails m of
                                        Right m' -> return m'
                                        Left (InvalidEmail e) -> fail $ "Invalid email: " ++ unpack e
    where
      checkEmails :: Map Text (Text, Text) -> Except UserCreationError UserDB
      checkEmails = flip foldlWithKey (return M.empty) $ \a k v -> a >>= addUser k (fst v) (snd v)

addUser :: Text -> Text -> Text -> UserDB -> Except UserCreationError UserDB
addUser email firstName lastName db =
  case emailAddress (encodeUtf8 email) of
    Just email' -> return $ insert email' (firstName, lastName) db
    Nothing     -> throwError $ InvalidEmail email


loadUsers :: FilePath -> ExceptT ParseException IO UserDB
loadUsers = ExceptT . decodeFileEither

{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module Facebook.User where

import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)


newtype FacebookUserAccessToken = FacebookUserAccessToken
  { getFacebookLoginUserAccessToken :: Text
  } deriving (Eq, Show, PersistField, PersistFieldSql, Hashable)

instance ToJSON FacebookUserAccessToken where
  toJSON (FacebookUserAccessToken x) = String x

instance FromJSON FacebookUserAccessToken where
  parseJSON (String x) = pure (FacebookUserAccessToken x)
  parseJSON x = typeMismatch "FacebookLoginUserAccessToken" x



newtype FacebookUserId = FacebookUserId
  { getFacebookUserId :: Text
  } deriving (Eq, Show, PersistField, PersistFieldSql, Hashable)

instance ToJSON FacebookUserId where
  toJSON (FacebookUserId x) = String x

instance FromJSON FacebookUserId where
  parseJSON (String x) = pure (FacebookUserId x)
  parseJSON x = typeMismatch "FacebookLoginUserId" x

{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Common.Password where

import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Base64 (base64)
import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Attoparsec (attoAeson)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as BS64
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)


newtype HashedPassword = HashedPassword
  { getHashedPassword :: ByteString
  } deriving (Eq, Show, PersistField, PersistFieldSql)

hashedPasswordParser :: Parser HashedPassword
hashedPasswordParser = HashedPassword . T.encodeUtf8 <$> base64

instance ToJSON HashedPassword where
  toJSON (HashedPassword x) = toJSON (T.decodeUtf8 (BS64.encode x))

instance FromJSON HashedPassword where
  parseJSON = attoAeson hashedPasswordParser

{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Common.AuthToken where

import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Base64 (base64)
import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Attoparsec (attoAeson)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as BS64
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Crypto.Saltine.Core.Box (newNonce)
import qualified Crypto.Saltine.Class as NaCl


newtype AuthToken = AuthToken
  { getAuthToken :: ByteString
  } deriving (Eq, Show, PersistField, PersistFieldSql, Hashable)

authTokenParser :: Parser AuthToken
authTokenParser = AuthToken . T.encodeUtf8 <$> base64

instance ToJSON AuthToken where
  toJSON = toJSON . printAuthToken

instance FromJSON AuthToken where
  parseJSON = attoAeson authTokenParser

printAuthToken :: AuthToken -> Text
printAuthToken (AuthToken x) = T.decodeUtf8 (BS64.encode x)


-- | Randomly generates a new one
genAuthToken :: IO AuthToken
genAuthToken = AuthToken . NaCl.encode <$> newNonce

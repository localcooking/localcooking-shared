{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Common.DeviceToken where

import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Base64 (base64)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson.Attoparsec (attoAeson)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as BS64
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Crypto.Saltine.Core.Box (newNonce)
import qualified Crypto.Saltine.Class as NaCl


newtype DeviceToken = DeviceToken
  { getDeviceToken :: ByteString
  } deriving (Eq, Show, PersistField, PersistFieldSql, Hashable)

deviceTokenParser :: Parser DeviceToken
deviceTokenParser = DeviceToken . T.encodeUtf8 <$> base64

instance ToJSON DeviceToken where
  toJSON (DeviceToken x) = toJSON (T.decodeUtf8 (BS64.encode x))

instance FromJSON DeviceToken where
  parseJSON = attoAeson deviceTokenParser


-- | Randomly generates a new one
genDeviceToken :: IO DeviceToken
genDeviceToken = DeviceToken . NaCl.encode <$> newNonce

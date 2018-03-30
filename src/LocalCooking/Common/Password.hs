{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module LocalCooking.Common.Password where

import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Base64 (base64)
import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Attoparsec (attoAeson)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), suchThat)
import Test.QuickCheck.Instances ()


newtype HashedPassword = HashedPassword
  { getHashedPassword :: ByteString
  } deriving (Eq, Show, PersistField, PersistFieldSql, Generic)

instance Arbitrary HashedPassword where
  arbitrary = HashedPassword <$> arbitrary `suchThat` (not . BS.null)

hashedPasswordParser :: Parser HashedPassword
hashedPasswordParser = do
  s <- base64
  case BS64.decode (T.encodeUtf8 s) of
    Left e -> fail (show e)
    Right x -> pure (HashedPassword x)


instance ToJSON HashedPassword where
  toJSON (HashedPassword x) = toJSON (T.decodeUtf8 (BS64.encode x))

instance FromJSON HashedPassword where
  parseJSON = attoAeson hashedPasswordParser

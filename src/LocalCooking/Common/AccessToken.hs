{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module LocalCooking.Common.AccessToken where

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
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()
import System.IO.Unsafe (unsafePerformIO)


newtype AccessToken = AccessToken
  { getAccessToken :: ByteString
  } deriving (Eq, Show, PersistField, PersistFieldSql, Hashable, Generic)

instance Arbitrary AccessToken where
  arbitrary =
    let x = unsafePerformIO genAccessToken
    in  x `seq` pure x

accessTokenParser :: Parser AccessToken
accessTokenParser = do
  s <- base64
  case BS64.decode (T.encodeUtf8 s) of
    Left e -> fail (show e)
    Right x -> pure (AccessToken x)

instance ToJSON AccessToken where
  toJSON = toJSON . printAccessToken

instance FromJSON AccessToken where
  parseJSON = attoAeson accessTokenParser

printAccessToken :: AccessToken -> Text
printAccessToken (AccessToken x) = T.decodeUtf8 (BS64.encode x)


-- | Randomly generates a new one
genAccessToken :: IO AccessToken
genAccessToken = AccessToken . NaCl.encode <$> newNonce

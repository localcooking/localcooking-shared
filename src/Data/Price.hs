{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module Data.Price where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String, Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Data.Hashable (Hashable)
import Database.Persist.TH (derivePersistField, derivePersistFieldJSON)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)



data Currency
  = USD
  deriving (Eq, Ord, Show, Read, Generic)
derivePersistField "Currency"

instance Arbitrary Currency where
  arbitrary = oneof
    [ pure USD
    ]

instance Hashable Currency

instance ToJSON Currency where
  toJSON x = String $ case x of
    USD -> "usd"

instance FromJSON Currency where
  parseJSON = attoAeson currencyParser


currencyParser :: Parser Currency
currencyParser = do
  let usd = USD <$ string "usd"
  usd


data Price = Price
  { priceCurrency :: Currency
  , priceAmount   :: Double
  } deriving (Eq, Show, Generic)
derivePersistFieldJSON "Price"

instance Arbitrary Price where
  arbitrary = Price <$> arbitrary <*> arbitrary

instance ToJSON Price where
  toJSON Price{..} = object
    [ "currency" .= priceCurrency
    , "amount" .= priceAmount
    ]

instance FromJSON Price where
  parseJSON json = case json of
    Object o -> Price <$> o .: "currency" <*> o .: "amount"
    _ -> typeMismatch "Price" json

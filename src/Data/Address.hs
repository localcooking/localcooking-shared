{-# LANGUAGE
    TemplateHaskell
  , RecordWildCards
  , OverloadedStrings
  #-}

module Data.Address where

import Data.Text (Text, unpack)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String, Object), (.=), (.:), object)
import Data.Aeson.Types (typeMismatch)
import Text.Read (readMaybe)
import Database.Persist.TH (derivePersistField, derivePersistFieldJSON)


data USAState
  = AL
  | AK
  | AZ
  | AR
  | CA
  | CO
  | CT
  | DE
  | FL
  | GA
  | HI
  | ID
  | IL
  | IN
  | IA
  | KS
  | KY
  | LA
  | ME
  | MD
  | MA
  | MI
  | MN
  | MS
  | MO
  | MT
  | NE
  | NV
  | NH
  | NJ
  | NM
  | NY
  | NC
  | ND
  | OH
  | OK
  | OR
  | PA
  | RI
  | SC
  | SD
  | TN
  | TX
  | UT
  | VT
  | VA
  | WA
  | WV
  | WI
  | WY
  deriving (Eq, Ord, Show, Read)

instance ToJSON USAState where
  toJSON = toJSON . show

instance FromJSON USAState where
  parseJSON json = case json of
    String s -> case readMaybe (unpack s) of
      Nothing -> fail'
      Just x -> pure x
    _ -> fail'
    where
      fail' = typeMismatch "USAState" json

derivePersistField "USAState"



data USAAddress = USAAddress
  { addressStreet :: Text
  , addressCity   :: Text
  , addressState  :: USAState
  , addressZip    :: Int
  } deriving (Eq, Show)

instance ToJSON USAAddress where
  toJSON USAAddress{..} = object
    [ "street" .= addressStreet
    , "city" .= addressCity
    , "state" .= addressState
    , "zip" .= addressZip
    ]

instance FromJSON USAAddress where
  parseJSON json = case json of
    Object o -> USAAddress <$> o .: "street"
                           <*> o .: "city"
                           <*> o .: "state"
                           <*> o .: "zip"
    _ -> fail'
    where
      fail' = typeMismatch "USAAddress" json

derivePersistFieldJSON "USAAddress"

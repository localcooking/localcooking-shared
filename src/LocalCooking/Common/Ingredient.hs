{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Common.Ingredient where

import LocalCooking.Common.Diet (Diet)

import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, Value (Object), (.=))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


-- in the database, stored ingredients should point (many-to-many) to the diets
-- they void, as an explicitly stored table `StoredIngredientVoids`

data Ingredient = Ingredient
  { ingredientName  :: Text
  , ingredientVoids :: [Diet]
  } deriving (Eq, Ord, Show, Generic)

instance Arbitrary Ingredient where
  arbitrary = Ingredient <$> arbitrary <*> arbitrary

instance ToJSON Ingredient where
  toJSON Ingredient{..} = object
    [ "name" .= ingredientName
    , "voids" .= ingredientVoids
    ]

instance FromJSON Ingredient where
  parseJSON json = case json of
    Object o -> Ingredient <$> o .: "name"
                           <*> o .: "voids"
    _ -> typeMismatch "Ingredient" json

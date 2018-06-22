{-# LANGUAGE
    DeriveGeneric
  , TemplateHaskell
  , OverloadedStrings
  #-}

module LocalCooking.Common.ContentRecord where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String, Object), object, (.:), (.=))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Data.Hashable (Hashable)
import Control.Applicative ((<|>))
import Database.Persist.TH (derivePersistField)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)



data TagRecordVariant
  = TagVariantChef
  | TagVariantCulture
  | TagVariantDiet
  | TagVariantFarm
  | TagVariantIngredient
  | TagVariantMeal
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)
derivePersistField "TagRecordVariant"

instance Arbitrary TagRecordVariant where
  arbitrary = oneof
    [ pure TagVariantChef
    , pure TagVariantCulture
    , pure TagVariantDiet
    , pure TagVariantFarm
    , pure TagVariantIngredient
    , pure TagVariantMeal
    ]

instance Hashable TagRecordVariant

instance ToJSON TagRecordVariant where
  toJSON x = String $ case x of
    TagVariantChef -> "chefTag"
    TagVariantCulture -> "cultureTag"
    TagVariantDiet -> "dietTag"
    TagVariantFarm -> "farmTag"
    TagVariantIngredient -> "ingredientTag"
    TagVariantMeal -> "mealTag"

instance FromJSON TagRecordVariant where
  parseJSON = attoAeson tagRecordVariantParser

tagRecordVariantParser :: Parser TagRecordVariant
tagRecordVariantParser = do
  let chef = TagVariantChef <$ string "chefTag"
      culture = TagVariantCulture <$ string "cultureTag"
      diet = TagVariantDiet <$ string "dietTag"
      farm = TagVariantFarm <$ string "farmTag"
      ingredient = TagVariantIngredient <$ string "ingredientTag"
      meal = TagVariantMeal <$ string "mealTag"
  chef <|> culture <|> diet <|> farm <|> ingredient <|> meal





-- | Top-level nullary storable variant declaring which type of content is stored
data ContentRecordVariant
  = TagRecordVariant TagRecordVariant
  deriving (Eq, Ord, Show, Read, Generic)
derivePersistField "ContentRecordVariant"

instance Enum ContentRecordVariant where
  fromEnum x = case x of
    TagRecordVariant y -> adjustTag (fromEnum y)
    where
      -- inclusive bounds for each type
      minBoundTag = fromEnum (minBound :: TagRecordVariant)
      maxBoundTag = fromEnum (maxBound :: TagRecordVariant)
      adjustTag j = j + 0 -- maxBoundTag + 1
  toEnum i
    |  i < 0 = minBound
    |  i >= minBoundTag
    && i <= maxBoundTag = TagRecordVariant (toEnum (adjustTag i))
    |  otherwise = maxBound
    where
      -- inclusive bounds for each type
      minBoundTag = fromEnum (minBound :: TagRecordVariant)
      maxBoundTag = fromEnum (maxBound :: TagRecordVariant)
      adjustTag j = j - 0 -- maxBoundTag + 1

instance Bounded ContentRecordVariant where
  minBound = TagRecordVariant TagVariantChef
  maxBound = TagRecordVariant TagVariantMeal

instance Arbitrary ContentRecordVariant where
  arbitrary = oneof
    [ TagRecordVariant <$> arbitrary
    ]

instance Hashable ContentRecordVariant

instance ToJSON ContentRecordVariant where
  toJSON x = case x of
    TagRecordVariant y -> object ["tag" .= y]

instance FromJSON ContentRecordVariant where
  parseJSON json = case json of
    Object o -> do
      let tag = TagRecordVariant <$> o .: "tag"
      tag
    _ -> typeMismatch "ContentRecordVariant" json

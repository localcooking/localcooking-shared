{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  , DeriveGeneric
  #-}

module LocalCooking.Common.Order where

import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Ingredient (Ingredient)

import Data.Time (UTCTime)
import Data.Price (Price)
import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Time.Calendar (Day)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Database.Persist.TH (derivePersistField)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()


data OrderProgress
  = PrepProgress
  | Cooking1Progress
  | Cooking2Progress
  | CookedProgress
  | FreezingProgress
  | PackagingProgress
  | DeliveringProgress
  | DeliveredProgress
  deriving (Eq, Ord, Enum, Show, Read, Generic)
derivePersistField "OrderProgress"

instance Arbitrary OrderProgress where
  arbitrary = oneof
    [ pure PrepProgress
    , pure Cooking1Progress
    , pure Cooking2Progress
    , pure CookedProgress
    , pure FreezingProgress
    , pure PackagingProgress
    , pure DeliveringProgress
    , pure DeliveredProgress
    ]

instance Hashable OrderProgress

instance ToJSON OrderProgress where
  toJSON x = String $ case x of
    PrepProgress -> "prep"
    Cooking1Progress -> "cook1"
    Cooking2Progress -> "cook2"
    CookedProgress -> "cooked"
    FreezingProgress -> "freezing"
    PackagingProgress -> "pack"
    DeliveringProgress -> "deliv"
    DeliveredProgress -> "done"

instance FromJSON OrderProgress where
  parseJSON = attoAeson orderProgressParser

orderProgressParser :: Parser OrderProgress
orderProgressParser = do
  let prep = PrepProgress <$ string "prep"
      cook1 = Cooking1Progress <$ string "cook1"
      cook2 = Cooking2Progress <$ string "cook2"
      cooked = CookedProgress <$ string "cooked"
      freezing = FreezingProgress <$ string "freezing"
      pack = PackagingProgress <$ string "pack"
      deliv = DeliveringProgress <$ string "deliv"
      done = DeliveredProgress <$ string "done"
  prep <|> cook1 <|> cook2 <|> cooked <|> freezing <|> pack <|> deliv <|> done

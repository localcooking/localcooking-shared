{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  , DeriveGeneric
  #-}

module LocalCooking.Common.Blog where

import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (String))
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Database.Persist.TH (derivePersistField)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()



data BlogPostVariant
  = CasualBlogPost
  | BusinessBlogPost
  | PersonalBlogPost
  deriving (Eq, Ord, Enum, Show, Read, Generic)
derivePersistField "BlogPostVariant"

instance Arbitrary BlogPostVariant where
  arbitrary = oneof
    [ pure CasualBlogPost
    , pure BusinessBlogPost
    , pure PersonalBlogPost
    ]

instance Hashable BlogPostVariant

instance ToJSON BlogPostVariant where
  toJSON x = String $ case x of
    CasualBlogPost -> "casual"
    BusinessBlogPost -> "business"
    PersonalBlogPost -> "personal"

instance FromJSON BlogPostVariant where
  parseJSON = attoAeson blogPostVariantParser

blogPostVariantParser :: Parser BlogPostVariant
blogPostVariantParser = do
  let casual = CasualBlogPost <$ string "casual"
      business = BusinessBlogPost <$ string "business"
      personal = PersonalBlogPost <$ string "personal"
  casual <|> business <|> personal

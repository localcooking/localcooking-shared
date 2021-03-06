{-# LANGUAGE
    DeriveGeneric
  , TemplateHaskell
  , OverloadedStrings
  #-}

module LocalCooking.Common.User.Role where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Data.Hashable (Hashable)
import Control.Applicative ((<|>))
import Database.Persist.TH (derivePersistField)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)



data UserRole
  = Customer
  | Chef
  | Farmer
  | Editor
  | Manager
  | Admin
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)
derivePersistField "UserRole"

instance Arbitrary UserRole where
  arbitrary = oneof
    [ pure Customer
    , pure Chef
    , pure Farmer
    , pure Editor
    , pure Manager
    , pure Admin
    ]

instance Hashable UserRole

instance ToJSON UserRole where
  toJSON x = String $ case x of
    Customer -> "customer"
    Chef     -> "chef"
    Farmer   -> "farmer"
    Editor   -> "editor"
    Manager  -> "manager"
    Admin    -> "admin"

instance FromJSON UserRole where
  parseJSON = attoAeson userRoleParser

userRoleParser :: Parser UserRole
userRoleParser = do
  let customer = Customer <$ string "customer"
      chef     = Chef <$ string "chef"
      farmer   = Farmer <$ string "farmer"
      editor   = Editor <$ string "editor"
      manager  = Manager <$ string "manager"
      admin    = Admin <$ string "admin"
  customer <|> chef <|> farmer <|> editor <|> manager <|> admin

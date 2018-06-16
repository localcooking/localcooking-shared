{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module LocalCooking.Common.User.Name where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.NonEmptyAlpha (NonEmptyAlphaText (..))
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Database.Persist.Class (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (listOf1)
import Test.QuickCheck.Instances ()



newtype Name = Name
  { getName :: NonEmpty Text
  } deriving (Eq, Ord, Hashable, Generic, ToJSON, FromJSON)

instance PersistField Name where
  toPersistValue (Name (n :| ns)) = toPersistValue ([n] ++ ns)
  fromPersistValue pv = do
    xs <- fromPersistValue pv
    case xs of
      n : ns -> pure $ Name $ n :| ns
      _ -> fail "Empty Name"

instance PersistFieldSql Name where
  sqlType Proxy = sqlType (Proxy :: Proxy [Text])

instance Show Name where
  show (Name (n :| ns)) = T.unpack $ T.unwords $ [n] ++ ns

instance Arbitrary Name where
  arbitrary = do
    x:xs <- listOf1 $ do
      NonEmptyAlphaText n <- arbitrary
      pure n
    pure $ Name $ x :| xs

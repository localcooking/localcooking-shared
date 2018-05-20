{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module LocalCooking.Semantic.Common where

import LocalCooking.Common.User.Password (HashedPassword)
import Facebook.Types (FacebookUserId, FacebookUserAccessToken)

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String, Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Data.Hashable (Hashable)
import Text.EmailAddress (EmailAddress)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)



-- | For supplying social login userId's to be recognized by LocalCooking -
-- used by user settings and register
data SocialLoginForm = SocialLoginForm
  { socialLoginFormFb :: Maybe FacebookUserId
  } deriving (Eq, Show, Generic)

instance Arbitrary SocialLoginForm where
  arbitrary = SocialLoginForm <$> arbitrary

instance ToJSON SocialLoginForm where
  toJSON SocialLoginForm{..} = object
    [ "fb" .= socialLoginFormFb
    ]

instance FromJSON SocialLoginForm where
  parseJSON json = case json of
    Object o -> SocialLoginForm <$> o .: "fb"
    _ -> typeMismatch "SocialLoginForm" json


-- | How a user sees themselves, across all apps. Roles are only visible to Admin,
--   while UserDetails dictate the availability of those roles
data User = User
  { userEmail          :: EmailAddress
  , userPassword       :: HashedPassword
  , userSocial         :: SocialLoginForm
  , userEmailConfirmed :: Bool
  } deriving (Eq, Show, Generic)

instance Arbitrary User where
  arbitrary = User <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ToJSON User where
  toJSON User{..} = object
    [ "email" .= userEmail
    , "password" .= userPassword
    , "social" .= userSocial
    , "emailConfirmed" .= userEmailConfirmed
    ]

instance FromJSON User where
  parseJSON json = case json of
    Object o -> User <$> o .: "email"
                     <*> o .: "password"
                     <*> o .: "social"
                     <*> o .: "emailConfirmed"
    _ -> typeMismatch "User" json



data Register = Register
  { registerEmail    :: EmailAddress
  , registerPassword :: HashedPassword
  , registerSocial   :: SocialLoginForm
  } deriving (Eq, Show, Generic)

instance Arbitrary Register where
  arbitrary = Register <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary

instance ToJSON Register where
  toJSON Register{..} = object
    [ "email" .= registerEmail
    , "password" .= registerPassword
    , "social" .= registerSocial
    ]

instance FromJSON Register where
  parseJSON json = case json of
    Object o -> Register <$> o .: "email"
                          <*> o .: "password"
                          <*> o .: "social"
    _ -> typeMismatch "Register" json


data Login = Login
  { loginEmail    :: EmailAddress
  , loginPassword :: HashedPassword
  } deriving (Eq, Show, Generic)

instance Arbitrary Login where
  arbitrary = Login <$> arbitrary
                    <*> arbitrary

instance ToJSON Login where
  toJSON Login{..} = object
    [ "email" .= loginEmail
    , "password" .= loginPassword
    ]

instance FromJSON Login where
  parseJSON json = case json of
    Object o -> Login <$> o .: "email"
                      <*> o .: "password"
    _ -> typeMismatch "Login" json


data SocialLogin
  = SocialLoginFB
    { socialLoginFBUserId :: FacebookUserId
    , socialLoginFBToken  :: FacebookUserAccessToken
    }
  deriving (Eq, Show, Generic)

instance Arbitrary SocialLogin where
  arbitrary = oneof
    [ SocialLoginFB <$> arbitrary
                    <*> arbitrary
    ]

instance ToJSON SocialLogin where
  toJSON x = case x of
    SocialLoginFB{..} -> object
      [ "fbUserId" .= socialLoginFBUserId
      , "fbToken" .= socialLoginFBToken
      ]

instance FromJSON SocialLogin where
  parseJSON json = case json of
    Object o ->
      let fb = SocialLoginFB <$> o .: "fbUserId"
                             <*> o .: "fbToken"
      in  fb
    _ -> typeMismatch "SocialLogin" json

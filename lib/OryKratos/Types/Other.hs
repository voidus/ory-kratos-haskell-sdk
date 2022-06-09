{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types.Other
  (Session(..)
  ,
  SettingsProfileFormConfig (..),
  SuccessfulSelfServiceLoginWithoutBrowser(..),
  SuccessfulSelfServiceRegistrationWithoutBrowser(..)
  )
where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))
import OryKratos.Types.Identity ( Identity )
import OryKratos.Types.Types
    ( SessionAuthenticationMethod, AuthenticatorAssuranceLevel )
import OryKratos.Types.Ui ( UiText, UiNode )
import OryKratos.Types.Helper ( removeFieldLabelPrefix )

-- | A Session
data Session = Session
  { sessionActive :: Maybe Bool -- ^ Active state. If false the session is no longer active.
  , sessionAuthenticatedUnderscoreat :: Maybe UTCTime -- ^ The Session Authentication Timestamp  When this session was authenticated at. If multi-factor authentication was used this is the time when the last factor was authenticated (e.g. the TOTP code challenge was completed).
  , sessionAuthenticationUnderscoremethods :: Maybe [SessionAuthenticationMethod] -- ^ A list of authenticators which were used to authenticate the session.
  , sessionAuthenticatorUnderscoreassuranceUnderscorelevel :: Maybe AuthenticatorAssuranceLevel -- ^ 
  , sessionExpiresUnderscoreat :: Maybe UTCTime -- ^ The Session Expiry  When this session expires at.
  , sessionId :: Text -- ^ 
  , sessionIdentity :: Identity -- ^ 
  , sessionIssuedUnderscoreat :: Maybe UTCTime -- ^ The Session Issuance Timestamp  When this session was issued at. Usually equal or close to `authenticated_at`.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON Session where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "session")
instance ToJSON Session where
  toJSON = genericToJSON (removeFieldLabelPrefix False "session")


-- | 
data SettingsProfileFormConfig = SettingsProfileFormConfig
  { settingsProfileFormConfigAction :: Text -- ^ Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
  , settingsProfileFormConfigMessages :: Maybe [UiText] -- ^ 
  , settingsProfileFormConfigMethod :: Text -- ^ Method is the form method (e.g. POST)
  , settingsProfileFormConfigNodes :: [UiNode] -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SettingsProfileFormConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "settingsProfileFormConfig")
instance ToJSON SettingsProfileFormConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "settingsProfileFormConfig")

-- | The Response for Login Flows via API
data SuccessfulSelfServiceLoginWithoutBrowser = SuccessfulSelfServiceLoginWithoutBrowser
  { successfulSelfServiceLoginWithoutBrowserSession :: Session -- ^ 
  , successfulSelfServiceLoginWithoutBrowserSessionUnderscoretoken :: Maybe Text -- ^ The Session Token  A session token is equivalent to a session cookie, but it can be sent in the HTTP Authorization Header:  Authorization: bearer ${session-token}  The session token is only issued for API flows, not for Browser flows!
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SuccessfulSelfServiceLoginWithoutBrowser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "successfulSelfServiceLoginWithoutBrowser")
instance ToJSON SuccessfulSelfServiceLoginWithoutBrowser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "successfulSelfServiceLoginWithoutBrowser")


-- | The Response for Registration Flows via API
data SuccessfulSelfServiceRegistrationWithoutBrowser = SuccessfulSelfServiceRegistrationWithoutBrowser
  { successfulSelfServiceRegistrationWithoutBrowserIdentity :: Identity -- ^ 
  , successfulSelfServiceRegistrationWithoutBrowserSession :: Maybe Session -- ^ 
  , successfulSelfServiceRegistrationWithoutBrowserSessionUnderscoretoken :: Maybe Text -- ^ The Session Token  This field is only set when the session hook is configured as a post-registration hook.  A session token is equivalent to a session cookie, but it can be sent in the HTTP Authorization Header:  Authorization: bearer ${session-token}  The session token is only issued for API flows, not for Browser flows!
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SuccessfulSelfServiceRegistrationWithoutBrowser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "successfulSelfServiceRegistrationWithoutBrowser")
instance ToJSON SuccessfulSelfServiceRegistrationWithoutBrowser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "successfulSelfServiceRegistrationWithoutBrowser")



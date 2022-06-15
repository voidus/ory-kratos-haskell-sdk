{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types.Other
  ( Session (..),
    SettingsProfileFormConfig (..),
    SuccessfulSelfServiceLoginWithoutBrowser (..),
    SuccessfulSelfServiceRegistrationWithoutBrowser (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import qualified Data.Char as Char
import Data.Data (Data)
import Data.Function ((&))
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.UUID (UUID)
import GHC.Generics (Generic)
import OryKratos.Types.Helper (removeFieldLabelPrefix)
import OryKratos.Types.Identity (Identity)
import OryKratos.Types.Types
  ( AuthenticatorAssuranceLevel,
    SessionAuthenticationMethod,
  )
import OryKratos.Types.Ui (UiNode, UiText)

-- | A Session
data Session = Session
  { -- | Active state. If false the session is no longer active.
    active :: Maybe Bool,
    -- | The Session Authentication Timestamp  When this session was authenticated at. If multi-factor authentication was used this is the time when the last factor was authenticated (e.g. the TOTP code challenge was completed).
    authenticated_at :: Maybe UTCTime,
    -- | A list of authenticators which were used to authenticate the session.
    authentication_methods :: Maybe [SessionAuthenticationMethod],
    authenticator_assurance_level :: Maybe AuthenticatorAssuranceLevel,
    -- | The Session Expiry  When this session expires at.
    expires_at :: Maybe UTCTime,
    id :: UUID,
    identity :: Identity,
    -- | The Session Issuance Timestamp  When this session was issued at. Usually equal or close to `authenticated_at`.
    issued_at :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Session

instance ToJSON Session where
  toEncoding = genericToEncoding defaultOptions

data SettingsProfileFormConfig = SettingsProfileFormConfig
  { -- | Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
    action :: Text,
    messages :: Maybe [UiText],
    -- | Method is the form method (e.g. POST)
    method :: Text,
    nodes :: [UiNode]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SettingsProfileFormConfig

instance ToJSON SettingsProfileFormConfig where
  toEncoding = genericToEncoding defaultOptions

-- | The Response for Login Flows via API
data SuccessfulSelfServiceLoginWithoutBrowser = SuccessfulSelfServiceLoginWithoutBrowser
  { session :: Session,
    -- | The Session Token  A session token is equivalent to a session cookie, but it can be sent in the HTTP Authorization Header:  Authorization: bearer ${session-token}  The session token is only issued for API flows, not for Browser flows!
    session_token :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SuccessfulSelfServiceLoginWithoutBrowser

instance ToJSON SuccessfulSelfServiceLoginWithoutBrowser where
  toEncoding = genericToEncoding defaultOptions

-- | The Response for Registration Flows via API
data SuccessfulSelfServiceRegistrationWithoutBrowser = SuccessfulSelfServiceRegistrationWithoutBrowser
  { identity :: Identity,
    session :: Maybe Session,
    -- | The Session Token  This field is only set when the session hook is configured as a post-registration hook.  A session token is equivalent to a session cookie, but it can be sent in the HTTP Authorization Header:  Authorization: bearer ${session-token}  The session token is only issued for API flows, not for Browser flows!
    session_token :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SuccessfulSelfServiceRegistrationWithoutBrowser

instance ToJSON SuccessfulSelfServiceRegistrationWithoutBrowser where
  toEncoding = genericToEncoding defaultOptions

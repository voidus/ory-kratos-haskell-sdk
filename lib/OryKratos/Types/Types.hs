{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types.Types
  ( AuthenticatorAssuranceLevel (..),
    ErrorAuthenticatorAssuranceLevelNotSatisfied (..),
    GenericError (..),
    GetVersion200Response (..),
    HealthNotReadyStatus (..),
    HealthStatus (..),
    IsAlive200Response (..),
    IsReady503Response (..),
    JsonError (..),
    NeedsPrivilegedSessionError (..),
    Pagination (..),
    RecoveryAddress (..),
    RevokedSessions (..),
    SessionAuthenticationMethod (..),
    SessionDevice (..),
    SubmitSelfServiceFlowWithWebAuthnRegistrationMethod (..),
    VerifiableIdentityAddress (..),
    Version (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, genericParseJSON, genericToEncoding, genericToJSON)
import qualified Data.Aeson as Aeson
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

-- | The authenticator assurance level can be one of \&quot;aal1\&quot;, \&quot;aal2\&quot;, or \&quot;aal3\&quot;. A higher number means that it is harder for an attacker to compromise the account.  Generally, \&quot;aal1\&quot; implies that one authentication factor was used while AAL2 implies that two factors (e.g. password + TOTP) have been used.  To learn more about these levels please head over to: https://www.ory.sh/kratos/docs/concepts/credentials
data AuthenticatorAssuranceLevel
  = AuthenticatorAssuranceLevel0
  | AuthenticatorAssuranceLevel1
  | AuthenticatorAssuranceLevel2
  | AuthenticatorAssuranceLevel3
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AuthenticatorAssuranceLevel where
  parseJSON (Aeson.String s) = case T.unpack s of
    "aal0" -> return AuthenticatorAssuranceLevel0
    "aal1" -> return AuthenticatorAssuranceLevel1
    "aal2" -> return AuthenticatorAssuranceLevel2
    "aal3" -> return AuthenticatorAssuranceLevel3
    _ -> Prelude.error "Invalid AuthenticatorAssuranceLevel"
  parseJSON _ = Prelude.error "Invalid AuthenticatorAssuranceLevel"

instance ToJSON AuthenticatorAssuranceLevel where
  toJSON (AuthenticatorAssuranceLevel0) = Aeson.String "aal0"
  toJSON (AuthenticatorAssuranceLevel1) = Aeson.String "aal1"
  toJSON (AuthenticatorAssuranceLevel2) = Aeson.String "aal2"
  toJSON (AuthenticatorAssuranceLevel3) = Aeson.String "aal3"

data ErrorAuthenticatorAssuranceLevelNotSatisfied = ErrorAuthenticatorAssuranceLevelNotSatisfied
  { -- | The status code
    code :: Maybe Integer,
    -- | Debug information  This field is often not exposed to protect against leaking sensitive information.
    debug :: Maybe Text,
    -- | Further error details
    details :: Maybe (Map.Map String Value),
    -- | The error ID  Useful when trying to identify various errors in application logic.
    id :: Maybe Text,
    -- | Error message  The error's message.
    message :: Text,
    -- | A human-readable reason for the error
    reason :: Maybe Text,
    redirect_browser_to :: Maybe Text,
    -- | The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
    request :: Maybe Text,
    -- | The status description
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ErrorAuthenticatorAssuranceLevelNotSatisfied where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "errorAuthenticatorAssuranceLevelNotSatisfied")

instance ToJSON ErrorAuthenticatorAssuranceLevelNotSatisfied where
  toJSON = genericToJSON (removeFieldLabelPrefix False "errorAuthenticatorAssuranceLevelNotSatisfied")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "errorAuthenticatorAssuranceLevelNotSatisfied")

data GenericError = GenericError
  { -- | The status code
    genericErrorCode :: Maybe Integer,
    -- | Debug information  This field is often not exposed to protect against leaking sensitive information.
    genericErrorDebug :: Maybe Text,
    -- | Further error details
    genericErrorDetails :: Maybe Value,
    -- | The error ID  Useful when trying to identify various errors in application logic.
    genericErrorId :: Maybe Text,
    -- | Error message  The error's message.
    genericErrorMessage :: Text,
    -- | A human-readable reason for the error
    genericErrorReason :: Maybe Text,
    -- | The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
    genericErrorRequest :: Maybe Text,
    -- | The status description
    genericErrorStatus :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON GenericError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "genericError")

instance ToJSON GenericError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "genericError")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "genericError")

data GetVersion200Response = GetVersion200Response
  { -- | The version of Ory Kratos.
    getVersion200ResponseVersion :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON GetVersion200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getVersion200Response")

instance ToJSON GetVersion200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getVersion200Response")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "getVersion200Response")

data HealthNotReadyStatus = HealthNotReadyStatus
  { -- | Errors contains a list of errors that caused the not ready status.
    healthNotReadyStatusErrors :: Maybe (Map.Map String Text)
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON HealthNotReadyStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "healthNotReadyStatus")

instance ToJSON HealthNotReadyStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "healthNotReadyStatus")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "healthNotReadyStatus")

data HealthStatus = HealthStatus
  { -- | Status always contains \"ok\".
    healthStatusStatus :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON HealthStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "healthStatus")

instance ToJSON HealthStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "healthStatus")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "healthStatus")

data IsAlive200Response = IsAlive200Response
  { -- | Always \"ok\".
    isAlive200ResponseStatus :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IsAlive200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "isAlive200Response")

instance ToJSON IsAlive200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "isAlive200Response")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "isAlive200Response")

data IsReady503Response = IsReady503Response
  { -- | Errors contains a list of errors that caused the not ready status.
    isReady503ResponseErrors :: (Map.Map String Text)
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IsReady503Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "isReady503Response")

instance ToJSON IsReady503Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "isReady503Response")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "isReady503Response")

-- | The standard Ory JSON API error format.
data JsonError = JsonError
  { jsonErrorError :: GenericError
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON JsonError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "jsonError")

instance ToJSON JsonError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "jsonError")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "jsonError")

data NeedsPrivilegedSessionError = NeedsPrivilegedSessionError
  { -- | The status code
    code :: Maybe Integer,
    -- | Debug information  This field is often not exposed to protect against leaking sensitive information.
    debug :: Maybe Text,
    -- | Further error details
    details :: Maybe (Map.Map String Value),
    -- | The error ID  Useful when trying to identify various errors in application logic.
    id :: Maybe Text,
    -- | Error message  The error's message.
    message :: Text,
    -- | A human-readable reason for the error
    reason :: Maybe Text,
    -- | Points to where to redirect the user to next.
    redirect_browser_to :: Text,
    -- | The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
    request :: Maybe Text,
    -- | The status description
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON NeedsPrivilegedSessionError

instance ToJSON NeedsPrivilegedSessionError where
  toEncoding = genericToEncoding defaultOptions

data Pagination = Pagination
  { -- | Pagination Page
    page :: Maybe Integer,
    -- | Items per Page  This is the number of items per page.
    per_page :: Maybe Integer
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Pagination

instance ToJSON Pagination where
  toEncoding = genericToEncoding defaultOptions

data RecoveryAddress = RecoveryAddress
  { -- | CreatedAt is a helper struct field for gobuffalo.pop.
    created_at :: Maybe UTCTime,
    id :: UUID,
    -- | UpdatedAt is a helper struct field for gobuffalo.pop.
    updated_at :: Maybe UTCTime,
    value :: Text,
    via :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RecoveryAddress

instance ToJSON RecoveryAddress where
  toEncoding = genericToEncoding defaultOptions

data RevokedSessions = RevokedSessions
  { -- | The number of sessions that were revoked.
    count :: Maybe Integer
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RevokedSessions

instance ToJSON RevokedSessions where
  toEncoding = genericToEncoding defaultOptions

-- | A singular authenticator used during authentication / login.
data SessionAuthenticationMethod = SessionAuthenticationMethod
  { aal :: Maybe AuthenticatorAssuranceLevel,
    -- | When the authentication challenge was completed.
    completed_at :: Maybe UTCTime,
    method :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SessionAuthenticationMethod

instance ToJSON SessionAuthenticationMethod where
  toEncoding = genericToEncoding defaultOptions

data SessionDevice = SessionDevice
  { -- | UserAgent of this device
    user_agent :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SessionDevice

instance ToJSON SessionDevice where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceFlowWithWebAuthnRegistrationMethod = SubmitSelfServiceFlowWithWebAuthnRegistrationMethod
  { -- | Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
    webauthn_register :: Maybe Text,
    -- | Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
    webauthn_register_displayname :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceFlowWithWebAuthnRegistrationMethod

instance ToJSON SubmitSelfServiceFlowWithWebAuthnRegistrationMethod where
  toEncoding = genericToEncoding defaultOptions

-- | VerifiableAddress is an identity&#39;s verifiable address
data VerifiableIdentityAddress = VerifiableIdentityAddress
  { -- | When this entry was created
    created_at :: Maybe UTCTime,
    id :: UUID,
    -- | VerifiableAddressStatus must not exceed 16 characters as that is the limitation in the SQL Schema
    status :: Text,
    -- | When this entry was last updated
    updated_at :: Maybe UTCTime,
    -- | The address value  example foo@user.com
    value :: Text,
    -- | Indicates if the address has already been verified
    verified :: Bool,
    verified_at :: Maybe UTCTime,
    -- | VerifiableAddressType must not exceed 16 characters as that is the limitation in the SQL Schema
    via :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerifiableIdentityAddress

instance ToJSON VerifiableIdentityAddress where
  toEncoding = genericToEncoding defaultOptions

data Version = Version
  { -- | Version is the service's version.
    version :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Version

instance ToJSON Version where
  toEncoding = genericToEncoding defaultOptions

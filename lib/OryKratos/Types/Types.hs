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

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON, genericToEncoding)
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
import OryKratos.Types.Helper ( removeFieldLabelPrefix )

-- | The authenticator assurance level can be one of \&quot;aal1\&quot;, \&quot;aal2\&quot;, or \&quot;aal3\&quot;. A higher number means that it is harder for an attacker to compromise the account.  Generally, \&quot;aal1\&quot; implies that one authentication factor was used while AAL2 implies that two factors (e.g. password + TOTP) have been used.  To learn more about these levels please head over to: https://www.ory.sh/kratos/docs/concepts/credentials
data AuthenticatorAssuranceLevel = AuthenticatorAssuranceLevel
  { 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AuthenticatorAssuranceLevel where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "authenticatorAssuranceLevel")
instance ToJSON AuthenticatorAssuranceLevel where
  toJSON = genericToJSON (removeFieldLabelPrefix False "authenticatorAssuranceLevel")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "authenticatorAssuranceLevel")


-- | 
data ErrorAuthenticatorAssuranceLevelNotSatisfied = ErrorAuthenticatorAssuranceLevelNotSatisfied
  { errorAuthenticatorAssuranceLevelNotSatisfiedCode :: Maybe Integer -- ^ The status code
  , errorAuthenticatorAssuranceLevelNotSatisfiedDebug :: Maybe Text -- ^ Debug information  This field is often not exposed to protect against leaking sensitive information.
  , errorAuthenticatorAssuranceLevelNotSatisfiedDetails :: Maybe (Map.Map String Value) -- ^ Further error details
  , errorAuthenticatorAssuranceLevelNotSatisfiedId :: Maybe Text -- ^ The error ID  Useful when trying to identify various errors in application logic.
  , errorAuthenticatorAssuranceLevelNotSatisfiedMessage :: Text -- ^ Error message  The error's message.
  , errorAuthenticatorAssuranceLevelNotSatisfiedReason :: Maybe Text -- ^ A human-readable reason for the error
  , errorAuthenticatorAssuranceLevelNotSatisfiedRedirectUnderscorebrowserUnderscoreto :: Maybe Text -- ^ 
  , errorAuthenticatorAssuranceLevelNotSatisfiedRequest :: Maybe Text -- ^ The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
  , errorAuthenticatorAssuranceLevelNotSatisfiedStatus :: Maybe Text -- ^ The status description
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON ErrorAuthenticatorAssuranceLevelNotSatisfied where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "errorAuthenticatorAssuranceLevelNotSatisfied")
instance ToJSON ErrorAuthenticatorAssuranceLevelNotSatisfied where
  toJSON = genericToJSON (removeFieldLabelPrefix False "errorAuthenticatorAssuranceLevelNotSatisfied")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "errorAuthenticatorAssuranceLevelNotSatisfied")


-- | 
data GenericError = GenericError
  { genericErrorCode :: Maybe Integer -- ^ The status code
  , genericErrorDebug :: Maybe Text -- ^ Debug information  This field is often not exposed to protect against leaking sensitive information.
  , genericErrorDetails :: Maybe Value -- ^ Further error details
  , genericErrorId :: Maybe Text -- ^ The error ID  Useful when trying to identify various errors in application logic.
  , genericErrorMessage :: Text -- ^ Error message  The error's message.
  , genericErrorReason :: Maybe Text -- ^ A human-readable reason for the error
  , genericErrorRequest :: Maybe Text -- ^ The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
  , genericErrorStatus :: Maybe Text -- ^ The status description
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON GenericError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "genericError")
instance ToJSON GenericError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "genericError")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "genericError")


-- | 
data GetVersion200Response = GetVersion200Response
  { getVersion200ResponseVersion :: Text -- ^ The version of Ory Kratos.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON GetVersion200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getVersion200Response")
instance ToJSON GetVersion200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getVersion200Response")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "getVersion200Response")


-- | 
data HealthNotReadyStatus = HealthNotReadyStatus
  { healthNotReadyStatusErrors :: Maybe (Map.Map String Text) -- ^ Errors contains a list of errors that caused the not ready status.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON HealthNotReadyStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "healthNotReadyStatus")
instance ToJSON HealthNotReadyStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "healthNotReadyStatus")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "healthNotReadyStatus")


-- | 
data HealthStatus = HealthStatus
  { healthStatusStatus :: Maybe Text -- ^ Status always contains \"ok\".
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON HealthStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "healthStatus")
instance ToJSON HealthStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "healthStatus")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "healthStatus")

-- | 
data IsAlive200Response = IsAlive200Response
  { isAlive200ResponseStatus :: Text -- ^ Always \"ok\".
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON IsAlive200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "isAlive200Response")
instance ToJSON IsAlive200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "isAlive200Response")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "isAlive200Response")


-- | 
data IsReady503Response = IsReady503Response
  { isReady503ResponseErrors :: (Map.Map String Text) -- ^ Errors contains a list of errors that caused the not ready status.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON IsReady503Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "isReady503Response")
instance ToJSON IsReady503Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "isReady503Response")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "isReady503Response")


-- | The standard Ory JSON API error format.
data JsonError = JsonError
  { jsonErrorError :: GenericError -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON JsonError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "jsonError")
instance ToJSON JsonError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "jsonError")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "jsonError")


-- | 
data NeedsPrivilegedSessionError = NeedsPrivilegedSessionError
  { needsPrivilegedSessionErrorCode :: Maybe Integer -- ^ The status code
  , needsPrivilegedSessionErrorDebug :: Maybe Text -- ^ Debug information  This field is often not exposed to protect against leaking sensitive information.
  , needsPrivilegedSessionErrorDetails :: Maybe (Map.Map String Value) -- ^ Further error details
  , needsPrivilegedSessionErrorId :: Maybe Text -- ^ The error ID  Useful when trying to identify various errors in application logic.
  , needsPrivilegedSessionErrorMessage :: Text -- ^ Error message  The error's message.
  , needsPrivilegedSessionErrorReason :: Maybe Text -- ^ A human-readable reason for the error
  , needsPrivilegedSessionErrorRedirectUnderscorebrowserUnderscoreto :: Text -- ^ Points to where to redirect the user to next.
  , needsPrivilegedSessionErrorRequest :: Maybe Text -- ^ The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
  , needsPrivilegedSessionErrorStatus :: Maybe Text -- ^ The status description
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON NeedsPrivilegedSessionError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "needsPrivilegedSessionError")
instance ToJSON NeedsPrivilegedSessionError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "needsPrivilegedSessionError")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "needsPrivilegedSessionError")


-- | 
data Pagination = Pagination
  { paginationPage :: Maybe Integer -- ^ Pagination Page
  , paginationPerUnderscorepage :: Maybe Integer -- ^ Items per Page  This is the number of items per page.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON Pagination where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pagination")
instance ToJSON Pagination where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pagination")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "pagination")


-- | 
data RecoveryAddress = RecoveryAddress
  { recoveryAddressCreatedUnderscoreat :: Maybe UTCTime -- ^ CreatedAt is a helper struct field for gobuffalo.pop.
  , recoveryAddressId :: Text -- ^ 
  , recoveryAddressUpdatedUnderscoreat :: Maybe UTCTime -- ^ UpdatedAt is a helper struct field for gobuffalo.pop.
  , recoveryAddressValue :: Text -- ^ 
  , recoveryAddressVia :: Text -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON RecoveryAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "recoveryAddress")
instance ToJSON RecoveryAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "recoveryAddress")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "recoveryAddress")


-- | 
data RevokedSessions = RevokedSessions
  { revokedSessionsCount :: Maybe Integer -- ^ The number of sessions that were revoked.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON RevokedSessions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "revokedSessions")
instance ToJSON RevokedSessions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "revokedSessions")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "revokedSessions")

-- | A singular authenticator used during authentication / login.
data SessionAuthenticationMethod = SessionAuthenticationMethod
  { sessionAuthenticationMethodAal :: Maybe AuthenticatorAssuranceLevel -- ^ 
  , sessionAuthenticationMethodCompletedUnderscoreat :: Maybe UTCTime -- ^ When the authentication challenge was completed.
  , sessionAuthenticationMethodMethod :: Maybe Text -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SessionAuthenticationMethod where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sessionAuthenticationMethod")
instance ToJSON SessionAuthenticationMethod where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sessionAuthenticationMethod")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "sessionAuthenticationMethod")


-- | 
data SessionDevice = SessionDevice
  { sessionDeviceUserUnderscoreagent :: Maybe Text -- ^ UserAgent of this device
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SessionDevice where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sessionDevice")
instance ToJSON SessionDevice where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sessionDevice")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "sessionDevice")

-- | 
data SubmitSelfServiceFlowWithWebAuthnRegistrationMethod = SubmitSelfServiceFlowWithWebAuthnRegistrationMethod
  { submitSelfServiceFlowWithWebAuthnRegistrationMethodWebauthnUnderscoreregister :: Maybe Text -- ^ Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
  , submitSelfServiceFlowWithWebAuthnRegistrationMethodWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text -- ^ Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceFlowWithWebAuthnRegistrationMethod where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceFlowWithWebAuthnRegistrationMethod")
instance ToJSON SubmitSelfServiceFlowWithWebAuthnRegistrationMethod where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceFlowWithWebAuthnRegistrationMethod")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceFlowWithWebAuthnRegistrationMethod")

-- | VerifiableAddress is an identity&#39;s verifiable address
data VerifiableIdentityAddress = VerifiableIdentityAddress
  { verifiableIdentityAddressCreatedUnderscoreat :: Maybe UTCTime -- ^ When this entry was created
  , verifiableIdentityAddressId :: Text -- ^ 
  , verifiableIdentityAddressStatus :: Text -- ^ VerifiableAddressStatus must not exceed 16 characters as that is the limitation in the SQL Schema
  , verifiableIdentityAddressUpdatedUnderscoreat :: Maybe UTCTime -- ^ When this entry was last updated
  , verifiableIdentityAddressValue :: Text -- ^ The address value  example foo@user.com
  , verifiableIdentityAddressVerified :: Bool -- ^ Indicates if the address has already been verified
  , verifiableIdentityAddressVerifiedUnderscoreat :: Maybe UTCTime -- ^ 
  , verifiableIdentityAddressVia :: Text -- ^ VerifiableAddressType must not exceed 16 characters as that is the limitation in the SQL Schema
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerifiableIdentityAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "verifiableIdentityAddress")
instance ToJSON VerifiableIdentityAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "verifiableIdentityAddress")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "verifiableIdentityAddress")


-- | 
data Version = Version
  { versionVersion :: Maybe Text -- ^ Version is the service's version.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON Version where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "version")
instance ToJSON Version where
  toJSON = genericToJSON (removeFieldLabelPrefix False "version")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "version")

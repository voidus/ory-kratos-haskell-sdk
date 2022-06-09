{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types (
  AdminCreateIdentityBody (..),
  AdminCreateIdentityImportCredentialsOidc (..),
  AdminCreateIdentityImportCredentialsOidcConfig (..),
  AdminCreateIdentityImportCredentialsOidcProvider (..),
  AdminCreateIdentityImportCredentialsPassword (..),
  AdminCreateIdentityImportCredentialsPasswordConfig (..),
  AdminCreateSelfServiceRecoveryLinkBody (..),
  AdminIdentityImportCredentials (..),
  AdminUpdateIdentityBody (..),
  AuthenticatorAssuranceLevel (..),
  ErrorAuthenticatorAssuranceLevelNotSatisfied (..),
  GenericError (..),
  GetVersion200Response (..),
  HealthNotReadyStatus (..),
  HealthStatus (..),
  Identity (..),
  IdentityCredentials (..),
  IdentityCredentialsOidc (..),
  IdentityCredentialsOidcProvider (..),
  IdentityCredentialsPassword (..),
  IdentityCredentialsType (..),
  IdentitySchema (..),
  IdentityState (..),
  IsAlive200Response (..),
  IsReady503Response (..),
  JsonError (..),
  NeedsPrivilegedSessionError (..),
  Pagination (..),
  RecoveryAddress (..),
  RevokedSessions (..),
  SelfServiceBrowserLocationChangeRequiredError (..),
  SelfServiceError (..),
  SelfServiceFlowExpiredError (..),
  SelfServiceLoginFlow (..),
  SelfServiceLogoutUrl (..),
  SelfServiceRecoveryFlow (..),
  SelfServiceRecoveryFlowState (..),
  SelfServiceRecoveryLink (..),
  SelfServiceRegistrationFlow (..),
  SelfServiceSettingsFlow (..),
  SelfServiceSettingsFlowState (..),
  SelfServiceVerificationFlow (..),
  SelfServiceVerificationFlowState (..),
  Session (..),
  SessionAuthenticationMethod (..),
  SessionDevice (..),
  SettingsProfileFormConfig (..),
  SubmitSelfServiceFlowWithWebAuthnRegistrationMethod (..),
  SubmitSelfServiceLoginFlowBody (..),
  SubmitSelfServiceLoginFlowWithLookupSecretMethodBody (..),
  SubmitSelfServiceLoginFlowWithOidcMethodBody (..),
  SubmitSelfServiceLoginFlowWithPasswordMethodBody (..),
  SubmitSelfServiceLoginFlowWithTotpMethodBody (..),
  SubmitSelfServiceLoginFlowWithWebAuthnMethodBody (..),
  SubmitSelfServiceLogoutFlowWithoutBrowserBody (..),
  SubmitSelfServiceRecoveryFlowBody (..),
  SubmitSelfServiceRecoveryFlowWithLinkMethodBody (..),
  SubmitSelfServiceRegistrationFlowBody (..),
  SubmitSelfServiceRegistrationFlowWithOidcMethodBody (..),
  SubmitSelfServiceRegistrationFlowWithPasswordMethodBody (..),
  SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody (..),
  SubmitSelfServiceSettingsFlowBody (..),
  SubmitSelfServiceSettingsFlowWithLookupMethodBody (..),
  SubmitSelfServiceSettingsFlowWithOidcMethodBody (..),
  SubmitSelfServiceSettingsFlowWithPasswordMethodBody (..),
  SubmitSelfServiceSettingsFlowWithProfileMethodBody (..),
  SubmitSelfServiceSettingsFlowWithTotpMethodBody (..),
  SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody (..),
  SubmitSelfServiceVerificationFlowBody (..),
  SubmitSelfServiceVerificationFlowWithLinkMethodBody (..),
  SuccessfulSelfServiceLoginWithoutBrowser (..),
  SuccessfulSelfServiceRegistrationWithoutBrowser (..),
  UiContainer (..),
  UiNode (..),
  UiNodeAnchorAttributes (..),
  UiNodeAttributes (..),
  UiNodeImageAttributes (..),
  UiNodeInputAttributes (..),
  UiNodeMeta (..),
  UiNodeScriptAttributes (..),
  UiNodeTextAttributes (..),
  UiText (..),
  VerifiableIdentityAddress (..),
  Version (..),
  ) where

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


-- | 
data AdminCreateIdentityBody = AdminCreateIdentityBody
  { adminCreateIdentityBodyCredentials :: Maybe AdminIdentityImportCredentials -- ^ 
  , adminCreateIdentityBodyMetadataUnderscoreadmin :: Maybe Value -- ^ Store metadata about the user which is only accessible through admin APIs such as `GET /admin/identities/<id>`.
  , adminCreateIdentityBodyMetadataUnderscorepublic :: Maybe Value -- ^ Store metadata about the identity which the identity itself can see when calling for example the session endpoint. Do not store sensitive information (e.g. credit score) about the identity in this field.
  , adminCreateIdentityBodyRecoveryUnderscoreaddresses :: Maybe [RecoveryAddress] -- ^ RecoveryAddresses contains all the addresses that can be used to recover an identity.  Use this structure to import recovery addresses for an identity. Please keep in mind that the address needs to be represented in the Identity Schema or this field will be overwritten on the next identity update.
  , adminCreateIdentityBodySchemaUnderscoreid :: Text -- ^ SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
  , adminCreateIdentityBodyState :: Maybe IdentityState -- ^ 
  , adminCreateIdentityBodyTraits :: Value -- ^ Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_url`.
  , adminCreateIdentityBodyVerifiableUnderscoreaddresses :: Maybe [VerifiableIdentityAddress] -- ^ VerifiableAddresses contains all the addresses that can be verified by the user.  Use this structure to import verified addresses for an identity. Please keep in mind that the address needs to be represented in the Identity Schema or this field will be overwritten on the next identity update.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityBody")
instance ToJSON AdminCreateIdentityBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityBody")


-- | 
data AdminCreateIdentityImportCredentialsOidc = AdminCreateIdentityImportCredentialsOidc
  { adminCreateIdentityImportCredentialsOidcConfig :: Maybe AdminCreateIdentityImportCredentialsOidcConfig -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsOidc where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsOidc")
instance ToJSON AdminCreateIdentityImportCredentialsOidc where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsOidc")


-- | 
data AdminCreateIdentityImportCredentialsOidcConfig = AdminCreateIdentityImportCredentialsOidcConfig
  { adminCreateIdentityImportCredentialsOidcConfigConfig :: Maybe AdminCreateIdentityImportCredentialsPasswordConfig -- ^ 
  , adminCreateIdentityImportCredentialsOidcConfigProviders :: Maybe [AdminCreateIdentityImportCredentialsOidcProvider] -- ^ A list of OpenID Connect Providers
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsOidcConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsOidcConfig")
instance ToJSON AdminCreateIdentityImportCredentialsOidcConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsOidcConfig")


-- | 
data AdminCreateIdentityImportCredentialsOidcProvider = AdminCreateIdentityImportCredentialsOidcProvider
  { adminCreateIdentityImportCredentialsOidcProviderProvider :: Text -- ^ The OpenID Connect provider to link the subject to. Usually something like `google` or `github`.
  , adminCreateIdentityImportCredentialsOidcProviderSubject :: Text -- ^ The subject (`sub`) of the OpenID Connect connection. Usually the `sub` field of the ID Token.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsOidcProvider where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsOidcProvider")
instance ToJSON AdminCreateIdentityImportCredentialsOidcProvider where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsOidcProvider")


-- | 
data AdminCreateIdentityImportCredentialsPassword = AdminCreateIdentityImportCredentialsPassword
  { adminCreateIdentityImportCredentialsPasswordConfig :: Maybe AdminCreateIdentityImportCredentialsPasswordConfig -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsPassword where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsPassword")
instance ToJSON AdminCreateIdentityImportCredentialsPassword where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsPassword")


-- | 
data AdminCreateIdentityImportCredentialsPasswordConfig = AdminCreateIdentityImportCredentialsPasswordConfig
  { adminCreateIdentityImportCredentialsPasswordConfigHashedUnderscorepassword :: Maybe Text -- ^ The hashed password in [PHC format]( https://www.ory.sh/docs/kratos/concepts/credentials/username-email-password#hashed-password-format)
  , adminCreateIdentityImportCredentialsPasswordConfigPassword :: Maybe Text -- ^ The password in plain text if no hash is available.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsPasswordConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsPasswordConfig")
instance ToJSON AdminCreateIdentityImportCredentialsPasswordConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsPasswordConfig")


-- | 
data AdminCreateSelfServiceRecoveryLinkBody = AdminCreateSelfServiceRecoveryLinkBody
  { adminCreateSelfServiceRecoveryLinkBodyExpiresUnderscorein :: Maybe Text -- ^ Link Expires In  The recovery link will expire at that point in time. Defaults to the configuration value of `selfservice.flows.recovery.request_lifespan`.
  , adminCreateSelfServiceRecoveryLinkBodyIdentityUnderscoreid :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminCreateSelfServiceRecoveryLinkBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateSelfServiceRecoveryLinkBody")
instance ToJSON AdminCreateSelfServiceRecoveryLinkBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateSelfServiceRecoveryLinkBody")


-- | 
data AdminIdentityImportCredentials = AdminIdentityImportCredentials
  { adminIdentityImportCredentialsOidc :: Maybe AdminCreateIdentityImportCredentialsOidc -- ^ 
  , adminIdentityImportCredentialsPassword :: Maybe AdminCreateIdentityImportCredentialsPassword -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminIdentityImportCredentials where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminIdentityImportCredentials")
instance ToJSON AdminIdentityImportCredentials where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminIdentityImportCredentials")


-- | 
data AdminUpdateIdentityBody = AdminUpdateIdentityBody
  { adminUpdateIdentityBodyMetadataUnderscoreadmin :: Maybe Value -- ^ Store metadata about the user which is only accessible through admin APIs such as `GET /admin/identities/<id>`.
  , adminUpdateIdentityBodyMetadataUnderscorepublic :: Maybe Value -- ^ Store metadata about the identity which the identity itself can see when calling for example the session endpoint. Do not store sensitive information (e.g. credit score) about the identity in this field.
  , adminUpdateIdentityBodySchemaUnderscoreid :: Text -- ^ SchemaID is the ID of the JSON Schema to be used for validating the identity's traits. If set will update the Identity's SchemaID.
  , adminUpdateIdentityBodyState :: IdentityState -- ^ 
  , adminUpdateIdentityBodyTraits :: Value -- ^ Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_id`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminUpdateIdentityBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminUpdateIdentityBody")
instance ToJSON AdminUpdateIdentityBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminUpdateIdentityBody")


-- | The authenticator assurance level can be one of \&quot;aal1\&quot;, \&quot;aal2\&quot;, or \&quot;aal3\&quot;. A higher number means that it is harder for an attacker to compromise the account.  Generally, \&quot;aal1\&quot; implies that one authentication factor was used while AAL2 implies that two factors (e.g. password + TOTP) have been used.  To learn more about these levels please head over to: https://www.ory.sh/kratos/docs/concepts/credentials
data AuthenticatorAssuranceLevel = AuthenticatorAssuranceLevel
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AuthenticatorAssuranceLevel where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "authenticatorAssuranceLevel")
instance ToJSON AuthenticatorAssuranceLevel where
  toJSON = genericToJSON (removeFieldLabelPrefix False "authenticatorAssuranceLevel")


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
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ErrorAuthenticatorAssuranceLevelNotSatisfied where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "errorAuthenticatorAssuranceLevelNotSatisfied")
instance ToJSON ErrorAuthenticatorAssuranceLevelNotSatisfied where
  toJSON = genericToJSON (removeFieldLabelPrefix False "errorAuthenticatorAssuranceLevelNotSatisfied")


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
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenericError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "genericError")
instance ToJSON GenericError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "genericError")


-- | 
data GetVersion200Response = GetVersion200Response
  { getVersion200ResponseVersion :: Text -- ^ The version of Ory Kratos.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetVersion200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getVersion200Response")
instance ToJSON GetVersion200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getVersion200Response")


-- | 
data HealthNotReadyStatus = HealthNotReadyStatus
  { healthNotReadyStatusErrors :: Maybe (Map.Map String Text) -- ^ Errors contains a list of errors that caused the not ready status.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON HealthNotReadyStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "healthNotReadyStatus")
instance ToJSON HealthNotReadyStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "healthNotReadyStatus")


-- | 
data HealthStatus = HealthStatus
  { healthStatusStatus :: Maybe Text -- ^ Status always contains \"ok\".
  } deriving (Show, Eq, Generic, Data)

instance FromJSON HealthStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "healthStatus")
instance ToJSON HealthStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "healthStatus")


-- | An identity can be a real human, a service, an IoT device - everything that can be described as an \&quot;actor\&quot; in a system.
data Identity = Identity
  { identityCreatedUnderscoreat :: Maybe UTCTime -- ^ CreatedAt is a helper struct field for gobuffalo.pop.
  , identityCredentials :: Maybe (Map.Map String IdentityCredentials) -- ^ Credentials represents all credentials that can be used for authenticating this identity.
  , identityId :: Text -- ^ 
  , identityMetadataUnderscoreadmin :: Maybe Value -- ^ NullJSONRawMessage represents a json.RawMessage that works well with JSON, SQL, and Swagger and is NULLable-
  , identityMetadataUnderscorepublic :: Maybe Value -- ^ NullJSONRawMessage represents a json.RawMessage that works well with JSON, SQL, and Swagger and is NULLable-
  , identityRecoveryUnderscoreaddresses :: Maybe [RecoveryAddress] -- ^ RecoveryAddresses contains all the addresses that can be used to recover an identity.
  , identitySchemaUnderscoreid :: Text -- ^ SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
  , identitySchemaUnderscoreurl :: Text -- ^ SchemaURL is the URL of the endpoint where the identity's traits schema can be fetched from.  format: url
  , identityState :: Maybe IdentityState -- ^ 
  , identityStateUnderscorechangedUnderscoreat :: Maybe UTCTime -- ^ 
  , identityTraits :: Value -- ^ Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_url`.
  , identityUpdatedUnderscoreat :: Maybe UTCTime -- ^ UpdatedAt is a helper struct field for gobuffalo.pop.
  , identityVerifiableUnderscoreaddresses :: Maybe [VerifiableIdentityAddress] -- ^ VerifiableAddresses contains all the addresses that can be verified by the user.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Identity where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identity")
instance ToJSON Identity where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identity")


-- | Credentials represents a specific credential type
data IdentityCredentials = IdentityCredentials
  { identityCredentialsConfig :: Maybe Value -- ^ 
  , identityCredentialsCreatedUnderscoreat :: Maybe UTCTime -- ^ CreatedAt is a helper struct field for gobuffalo.pop.
  , identityCredentialsIdentifiers :: Maybe [Text] -- ^ Identifiers represents a list of unique identifiers this credential type matches.
  , identityCredentialsType :: Maybe IdentityCredentialsType -- ^ 
  , identityCredentialsUpdatedUnderscoreat :: Maybe UTCTime -- ^ UpdatedAt is a helper struct field for gobuffalo.pop.
  , identityCredentialsVersion :: Maybe Integer -- ^ Version refers to the version of the credential. Useful when changing the config schema.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentials where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentials")
instance ToJSON IdentityCredentials where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentials")


-- | 
data IdentityCredentialsOidc = IdentityCredentialsOidc
  { identityCredentialsOidcProviders :: Maybe [IdentityCredentialsOidcProvider] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsOidc where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentialsOidc")
instance ToJSON IdentityCredentialsOidc where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentialsOidc")


-- | 
data IdentityCredentialsOidcProvider = IdentityCredentialsOidcProvider
  { identityCredentialsOidcProviderInitialUnderscoreaccessUnderscoretoken :: Maybe Text -- ^ 
  , identityCredentialsOidcProviderInitialUnderscoreidUnderscoretoken :: Maybe Text -- ^ 
  , identityCredentialsOidcProviderInitialUnderscorerefreshUnderscoretoken :: Maybe Text -- ^ 
  , identityCredentialsOidcProviderProvider :: Maybe Text -- ^ 
  , identityCredentialsOidcProviderSubject :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsOidcProvider where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentialsOidcProvider")
instance ToJSON IdentityCredentialsOidcProvider where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentialsOidcProvider")


-- | 
data IdentityCredentialsPassword = IdentityCredentialsPassword
  { identityCredentialsPasswordHashedUnderscorepassword :: Maybe Text -- ^ HashedPassword is a hash-representation of the password.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsPassword where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentialsPassword")
instance ToJSON IdentityCredentialsPassword where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentialsPassword")


-- | and so on.
data IdentityCredentialsType = IdentityCredentialsType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentialsType")
instance ToJSON IdentityCredentialsType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentialsType")


-- | 
data IdentitySchema = IdentitySchema
  { identitySchemaId :: Maybe Text -- ^ The ID of the Identity JSON Schema
  , identitySchemaSchema :: Maybe Value -- ^ The actual Identity JSON Schema
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentitySchema where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identitySchema")
instance ToJSON IdentitySchema where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identitySchema")


-- | The state can either be &#x60;active&#x60; or &#x60;inactive&#x60;.
data IdentityState = IdentityState
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentityState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityState")
instance ToJSON IdentityState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityState")


-- | 
data IsAlive200Response = IsAlive200Response
  { isAlive200ResponseStatus :: Text -- ^ Always \"ok\".
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IsAlive200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "isAlive200Response")
instance ToJSON IsAlive200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "isAlive200Response")


-- | 
data IsReady503Response = IsReady503Response
  { isReady503ResponseErrors :: (Map.Map String Text) -- ^ Errors contains a list of errors that caused the not ready status.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IsReady503Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "isReady503Response")
instance ToJSON IsReady503Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "isReady503Response")


-- | The standard Ory JSON API error format.
data JsonError = JsonError
  { jsonErrorError :: GenericError -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON JsonError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "jsonError")
instance ToJSON JsonError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "jsonError")


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
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NeedsPrivilegedSessionError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "needsPrivilegedSessionError")
instance ToJSON NeedsPrivilegedSessionError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "needsPrivilegedSessionError")


-- | 
data Pagination = Pagination
  { paginationPage :: Maybe Integer -- ^ Pagination Page
  , paginationPerUnderscorepage :: Maybe Integer -- ^ Items per Page  This is the number of items per page.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Pagination where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pagination")
instance ToJSON Pagination where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pagination")


-- | 
data RecoveryAddress = RecoveryAddress
  { recoveryAddressCreatedUnderscoreat :: Maybe UTCTime -- ^ CreatedAt is a helper struct field for gobuffalo.pop.
  , recoveryAddressId :: Text -- ^ 
  , recoveryAddressUpdatedUnderscoreat :: Maybe UTCTime -- ^ UpdatedAt is a helper struct field for gobuffalo.pop.
  , recoveryAddressValue :: Text -- ^ 
  , recoveryAddressVia :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RecoveryAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "recoveryAddress")
instance ToJSON RecoveryAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "recoveryAddress")


-- | 
data RevokedSessions = RevokedSessions
  { revokedSessionsCount :: Maybe Integer -- ^ The number of sessions that were revoked.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RevokedSessions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "revokedSessions")
instance ToJSON RevokedSessions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "revokedSessions")


-- | 
data SelfServiceBrowserLocationChangeRequiredError = SelfServiceBrowserLocationChangeRequiredError
  { selfServiceBrowserLocationChangeRequiredErrorCode :: Maybe Integer -- ^ The status code
  , selfServiceBrowserLocationChangeRequiredErrorDebug :: Maybe Text -- ^ Debug information  This field is often not exposed to protect against leaking sensitive information.
  , selfServiceBrowserLocationChangeRequiredErrorDetails :: Maybe (Map.Map String Value) -- ^ Further error details
  , selfServiceBrowserLocationChangeRequiredErrorId :: Maybe Text -- ^ The error ID  Useful when trying to identify various errors in application logic.
  , selfServiceBrowserLocationChangeRequiredErrorMessage :: Text -- ^ Error message  The error's message.
  , selfServiceBrowserLocationChangeRequiredErrorReason :: Maybe Text -- ^ A human-readable reason for the error
  , selfServiceBrowserLocationChangeRequiredErrorRedirectUnderscorebrowserUnderscoreto :: Maybe Text -- ^ Since when the flow has expired
  , selfServiceBrowserLocationChangeRequiredErrorRequest :: Maybe Text -- ^ The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
  , selfServiceBrowserLocationChangeRequiredErrorStatus :: Maybe Text -- ^ The status description
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceBrowserLocationChangeRequiredError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceBrowserLocationChangeRequiredError")
instance ToJSON SelfServiceBrowserLocationChangeRequiredError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceBrowserLocationChangeRequiredError")


-- | 
data SelfServiceError = SelfServiceError
  { selfServiceErrorCreatedUnderscoreat :: Maybe UTCTime -- ^ CreatedAt is a helper struct field for gobuffalo.pop.
  , selfServiceErrorError :: Maybe Value -- ^ 
  , selfServiceErrorId :: Text -- ^ 
  , selfServiceErrorUpdatedUnderscoreat :: Maybe UTCTime -- ^ UpdatedAt is a helper struct field for gobuffalo.pop.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceError")
instance ToJSON SelfServiceError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceError")


-- | Is sent when a flow is expired
data SelfServiceFlowExpiredError = SelfServiceFlowExpiredError
  { selfServiceFlowExpiredErrorCode :: Maybe Integer -- ^ The status code
  , selfServiceFlowExpiredErrorDebug :: Maybe Text -- ^ Debug information  This field is often not exposed to protect against leaking sensitive information.
  , selfServiceFlowExpiredErrorDetails :: Maybe (Map.Map String Value) -- ^ Further error details
  , selfServiceFlowExpiredErrorId :: Maybe Text -- ^ The error ID  Useful when trying to identify various errors in application logic.
  , selfServiceFlowExpiredErrorMessage :: Text -- ^ Error message  The error's message.
  , selfServiceFlowExpiredErrorReason :: Maybe Text -- ^ A human-readable reason for the error
  , selfServiceFlowExpiredErrorRequest :: Maybe Text -- ^ The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
  , selfServiceFlowExpiredErrorSince :: Maybe Integer -- ^ A Duration represents the elapsed time between two instants as an int64 nanosecond count. The representation limits the largest representable duration to approximately 290 years.
  , selfServiceFlowExpiredErrorStatus :: Maybe Text -- ^ The status description
  , selfServiceFlowExpiredErrorUseUnderscoreflowUnderscoreid :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceFlowExpiredError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceFlowExpiredError")
instance ToJSON SelfServiceFlowExpiredError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceFlowExpiredError")


-- | This object represents a login flow. A login flow is initiated at the \&quot;Initiate Login API / Browser Flow\&quot; endpoint by a client.  Once a login flow is completed successfully, a session cookie or session token will be issued.
data SelfServiceLoginFlow = SelfServiceLoginFlow
  { selfServiceLoginFlowActive :: Maybe IdentityCredentialsType -- ^ 
  , selfServiceLoginFlowCreatedUnderscoreat :: Maybe UTCTime -- ^ CreatedAt is a helper struct field for gobuffalo.pop.
  , selfServiceLoginFlowExpiresUnderscoreat :: UTCTime -- ^ ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to log in, a new flow has to be initiated.
  , selfServiceLoginFlowId :: Text -- ^ 
  , selfServiceLoginFlowIssuedUnderscoreat :: UTCTime -- ^ IssuedAt is the time (UTC) when the flow started.
  , selfServiceLoginFlowRefresh :: Maybe Bool -- ^ Refresh stores whether this login flow should enforce re-authentication.
  , selfServiceLoginFlowRequestUnderscoreurl :: Text -- ^ RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
  , selfServiceLoginFlowRequestedUnderscoreaal :: Maybe AuthenticatorAssuranceLevel -- ^ 
  , selfServiceLoginFlowReturnUnderscoreto :: Maybe Text -- ^ ReturnTo contains the requested return_to URL.
  , selfServiceLoginFlowType :: Text -- ^ The flow type can either be `api` or `browser`.
  , selfServiceLoginFlowUi :: UiContainer -- ^ 
  , selfServiceLoginFlowUpdatedUnderscoreat :: Maybe UTCTime -- ^ UpdatedAt is a helper struct field for gobuffalo.pop.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceLoginFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceLoginFlow")
instance ToJSON SelfServiceLoginFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceLoginFlow")


-- | 
data SelfServiceLogoutUrl = SelfServiceLogoutUrl
  { selfServiceLogoutUrlLogoutUnderscoretoken :: Text -- ^ LogoutToken can be used to perform logout using AJAX.
  , selfServiceLogoutUrlLogoutUnderscoreurl :: Text -- ^ LogoutURL can be opened in a browser to sign the user out.  format: uri
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceLogoutUrl where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceLogoutUrl")
instance ToJSON SelfServiceLogoutUrl where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceLogoutUrl")


-- | This request is used when an identity wants to recover their account.  We recommend reading the [Account Recovery Documentation](../self-service/flows/password-reset-account-recovery)
data SelfServiceRecoveryFlow = SelfServiceRecoveryFlow
  { selfServiceRecoveryFlowActive :: Maybe Text -- ^ Active, if set, contains the registration method that is being used. It is initially not set.
  , selfServiceRecoveryFlowExpiresUnderscoreat :: UTCTime -- ^ ExpiresAt is the time (UTC) when the request expires. If the user still wishes to update the setting, a new request has to be initiated.
  , selfServiceRecoveryFlowId :: Text -- ^ 
  , selfServiceRecoveryFlowIssuedUnderscoreat :: UTCTime -- ^ IssuedAt is the time (UTC) when the request occurred.
  , selfServiceRecoveryFlowRequestUnderscoreurl :: Text -- ^ RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
  , selfServiceRecoveryFlowReturnUnderscoreto :: Maybe Text -- ^ ReturnTo contains the requested return_to URL.
  , selfServiceRecoveryFlowState :: SelfServiceRecoveryFlowState -- ^ 
  , selfServiceRecoveryFlowType :: Text -- ^ The flow type can either be `api` or `browser`.
  , selfServiceRecoveryFlowUi :: UiContainer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRecoveryFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceRecoveryFlow")
instance ToJSON SelfServiceRecoveryFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceRecoveryFlow")


-- | The state represents the state of the recovery flow.  choose_method: ask the user to choose a method (e.g. recover account via email) sent_email: the email has been sent to the user passed_challenge: the request was successful and the recovery challenge was passed.
data SelfServiceRecoveryFlowState = SelfServiceRecoveryFlowState
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRecoveryFlowState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceRecoveryFlowState")
instance ToJSON SelfServiceRecoveryFlowState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceRecoveryFlowState")


-- | 
data SelfServiceRecoveryLink = SelfServiceRecoveryLink
  { selfServiceRecoveryLinkExpiresUnderscoreat :: Maybe UTCTime -- ^ Recovery Link Expires At  The timestamp when the recovery link expires.
  , selfServiceRecoveryLinkRecoveryUnderscorelink :: Text -- ^ Recovery Link  This link can be used to recover the account.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRecoveryLink where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceRecoveryLink")
instance ToJSON SelfServiceRecoveryLink where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceRecoveryLink")


-- | 
data SelfServiceRegistrationFlow = SelfServiceRegistrationFlow
  { selfServiceRegistrationFlowActive :: Maybe IdentityCredentialsType -- ^ 
  , selfServiceRegistrationFlowExpiresUnderscoreat :: UTCTime -- ^ ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to log in, a new flow has to be initiated.
  , selfServiceRegistrationFlowId :: Text -- ^ 
  , selfServiceRegistrationFlowIssuedUnderscoreat :: UTCTime -- ^ IssuedAt is the time (UTC) when the flow occurred.
  , selfServiceRegistrationFlowRequestUnderscoreurl :: Text -- ^ RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
  , selfServiceRegistrationFlowReturnUnderscoreto :: Maybe Text -- ^ ReturnTo contains the requested return_to URL.
  , selfServiceRegistrationFlowType :: Text -- ^ The flow type can either be `api` or `browser`.
  , selfServiceRegistrationFlowUi :: UiContainer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRegistrationFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceRegistrationFlow")
instance ToJSON SelfServiceRegistrationFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceRegistrationFlow")


-- | This flow is used when an identity wants to update settings (e.g. profile data, passwords, ...) in a selfservice manner.  We recommend reading the [User Settings Documentation](../self-service/flows/user-settings)
data SelfServiceSettingsFlow = SelfServiceSettingsFlow
  { selfServiceSettingsFlowActive :: Maybe Text -- ^ Active, if set, contains the registration method that is being used. It is initially not set.
  , selfServiceSettingsFlowExpiresUnderscoreat :: UTCTime -- ^ ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to update the setting, a new flow has to be initiated.
  , selfServiceSettingsFlowId :: Text -- ^ 
  , selfServiceSettingsFlowIdentity :: Identity -- ^ 
  , selfServiceSettingsFlowIssuedUnderscoreat :: UTCTime -- ^ IssuedAt is the time (UTC) when the flow occurred.
  , selfServiceSettingsFlowRequestUnderscoreurl :: Text -- ^ RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
  , selfServiceSettingsFlowReturnUnderscoreto :: Maybe Text -- ^ ReturnTo contains the requested return_to URL.
  , selfServiceSettingsFlowState :: SelfServiceSettingsFlowState -- ^ 
  , selfServiceSettingsFlowType :: Text -- ^ The flow type can either be `api` or `browser`.
  , selfServiceSettingsFlowUi :: UiContainer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceSettingsFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceSettingsFlow")
instance ToJSON SelfServiceSettingsFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceSettingsFlow")


-- | show_form: No user data has been collected, or it is invalid, and thus the form should be shown. success: Indicates that the settings flow has been updated successfully with the provided data. Done will stay true when repeatedly checking. If set to true, done will revert back to false only when a flow with invalid (e.g. \&quot;please use a valid phone number\&quot;) data was sent.
data SelfServiceSettingsFlowState = SelfServiceSettingsFlowState
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceSettingsFlowState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceSettingsFlowState")
instance ToJSON SelfServiceSettingsFlowState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceSettingsFlowState")


-- | Used to verify an out-of-band communication channel such as an email address or a phone number.  For more information head over to: https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation
data SelfServiceVerificationFlow = SelfServiceVerificationFlow
  { selfServiceVerificationFlowActive :: Maybe Text -- ^ Active, if set, contains the registration method that is being used. It is initially not set.
  , selfServiceVerificationFlowExpiresUnderscoreat :: Maybe UTCTime -- ^ ExpiresAt is the time (UTC) when the request expires. If the user still wishes to verify the address, a new request has to be initiated.
  , selfServiceVerificationFlowId :: Text -- ^ 
  , selfServiceVerificationFlowIssuedUnderscoreat :: Maybe UTCTime -- ^ IssuedAt is the time (UTC) when the request occurred.
  , selfServiceVerificationFlowRequestUnderscoreurl :: Maybe Text -- ^ RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
  , selfServiceVerificationFlowReturnUnderscoreto :: Maybe Text -- ^ ReturnTo contains the requested return_to URL.
  , selfServiceVerificationFlowState :: SelfServiceVerificationFlowState -- ^ 
  , selfServiceVerificationFlowType :: Text -- ^ The flow type can either be `api` or `browser`.
  , selfServiceVerificationFlowUi :: UiContainer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceVerificationFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceVerificationFlow")
instance ToJSON SelfServiceVerificationFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceVerificationFlow")


-- | The state represents the state of the verification flow.  choose_method: ask the user to choose a method (e.g. recover account via email) sent_email: the email has been sent to the user passed_challenge: the request was successful and the recovery challenge was passed.
data SelfServiceVerificationFlowState = SelfServiceVerificationFlowState
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelfServiceVerificationFlowState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceVerificationFlowState")
instance ToJSON SelfServiceVerificationFlowState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceVerificationFlowState")


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
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Session where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "session")
instance ToJSON Session where
  toJSON = genericToJSON (removeFieldLabelPrefix False "session")


-- | A singular authenticator used during authentication / login.
data SessionAuthenticationMethod = SessionAuthenticationMethod
  { sessionAuthenticationMethodAal :: Maybe AuthenticatorAssuranceLevel -- ^ 
  , sessionAuthenticationMethodCompletedUnderscoreat :: Maybe UTCTime -- ^ When the authentication challenge was completed.
  , sessionAuthenticationMethodMethod :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SessionAuthenticationMethod where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sessionAuthenticationMethod")
instance ToJSON SessionAuthenticationMethod where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sessionAuthenticationMethod")


-- | 
data SessionDevice = SessionDevice
  { sessionDeviceUserUnderscoreagent :: Maybe Text -- ^ UserAgent of this device
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SessionDevice where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sessionDevice")
instance ToJSON SessionDevice where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sessionDevice")


-- | 
data SettingsProfileFormConfig = SettingsProfileFormConfig
  { settingsProfileFormConfigAction :: Text -- ^ Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
  , settingsProfileFormConfigMessages :: Maybe [UiText] -- ^ 
  , settingsProfileFormConfigMethod :: Text -- ^ Method is the form method (e.g. POST)
  , settingsProfileFormConfigNodes :: [UiNode] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SettingsProfileFormConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "settingsProfileFormConfig")
instance ToJSON SettingsProfileFormConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "settingsProfileFormConfig")


-- | 
data SubmitSelfServiceFlowWithWebAuthnRegistrationMethod = SubmitSelfServiceFlowWithWebAuthnRegistrationMethod
  { submitSelfServiceFlowWithWebAuthnRegistrationMethodWebauthnUnderscoreregister :: Maybe Text -- ^ Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
  , submitSelfServiceFlowWithWebAuthnRegistrationMethodWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text -- ^ Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceFlowWithWebAuthnRegistrationMethod where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceFlowWithWebAuthnRegistrationMethod")
instance ToJSON SubmitSelfServiceFlowWithWebAuthnRegistrationMethod where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceFlowWithWebAuthnRegistrationMethod")


-- | 
data SubmitSelfServiceLoginFlowBody = SubmitSelfServiceLoginFlowBody
  { submitSelfServiceLoginFlowBodyCsrfUnderscoretoken :: Maybe Text -- ^ Sending the anti-csrf token is only required for browser login flows.
  , submitSelfServiceLoginFlowBodyIdentifier :: Text -- ^ Identifier is the email or username of the user trying to log in. This field is only required when using WebAuthn for passwordless login. When using WebAuthn for multi-factor authentication, it is not needed.
  , submitSelfServiceLoginFlowBodyMethod :: Text -- ^ Method should be set to \"lookup_secret\" when logging in using the lookup_secret strategy.
  , submitSelfServiceLoginFlowBodyPassword :: Text -- ^ The user's password.
  , submitSelfServiceLoginFlowBodyPasswordUnderscoreidentifier :: Maybe Text -- ^ Identifier is the email or username of the user trying to log in. This field is deprecated!
  , submitSelfServiceLoginFlowBodyProvider :: Text -- ^ The provider to register with
  , submitSelfServiceLoginFlowBodyTraits :: Maybe Value -- ^ The identity traits. This is a placeholder for the registration flow.
  , submitSelfServiceLoginFlowBodyTotpUnderscorecode :: Text -- ^ The TOTP code.
  , submitSelfServiceLoginFlowBodyWebauthnUnderscorelogin :: Maybe Text -- ^ Login a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
  , submitSelfServiceLoginFlowBodyLookupUnderscoresecret :: Text -- ^ The lookup secret.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowBody")
instance ToJSON SubmitSelfServiceLoginFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowBody")


-- | 
data SubmitSelfServiceLoginFlowWithLookupSecretMethodBody = SubmitSelfServiceLoginFlowWithLookupSecretMethodBody
  { submitSelfServiceLoginFlowWithLookupSecretMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ Sending the anti-csrf token is only required for browser login flows.
  , submitSelfServiceLoginFlowWithLookupSecretMethodBodyLookupUnderscoresecret :: Text -- ^ The lookup secret.
  , submitSelfServiceLoginFlowWithLookupSecretMethodBodyMethod :: Text -- ^ Method should be set to \"lookup_secret\" when logging in using the lookup_secret strategy.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithLookupSecretMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithLookupSecretMethodBody")
instance ToJSON SubmitSelfServiceLoginFlowWithLookupSecretMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithLookupSecretMethodBody")


-- | SubmitSelfServiceLoginFlowWithOidcMethodBody is used to decode the login form payload when using the oidc method.
data SubmitSelfServiceLoginFlowWithOidcMethodBody = SubmitSelfServiceLoginFlowWithOidcMethodBody
  { submitSelfServiceLoginFlowWithOidcMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ The CSRF Token
  , submitSelfServiceLoginFlowWithOidcMethodBodyMethod :: Text -- ^ Method to use  This field must be set to `oidc` when using the oidc method.
  , submitSelfServiceLoginFlowWithOidcMethodBodyProvider :: Text -- ^ The provider to register with
  , submitSelfServiceLoginFlowWithOidcMethodBodyTraits :: Maybe Value -- ^ The identity traits. This is a placeholder for the registration flow.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithOidcMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithOidcMethodBody")
instance ToJSON SubmitSelfServiceLoginFlowWithOidcMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithOidcMethodBody")


-- | 
data SubmitSelfServiceLoginFlowWithPasswordMethodBody = SubmitSelfServiceLoginFlowWithPasswordMethodBody
  { submitSelfServiceLoginFlowWithPasswordMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ Sending the anti-csrf token is only required for browser login flows.
  , submitSelfServiceLoginFlowWithPasswordMethodBodyIdentifier :: Text -- ^ Identifier is the email or username of the user trying to log in.
  , submitSelfServiceLoginFlowWithPasswordMethodBodyMethod :: Text -- ^ Method should be set to \"password\" when logging in using the identifier and password strategy.
  , submitSelfServiceLoginFlowWithPasswordMethodBodyPassword :: Text -- ^ The user's password.
  , submitSelfServiceLoginFlowWithPasswordMethodBodyPasswordUnderscoreidentifier :: Maybe Text -- ^ Identifier is the email or username of the user trying to log in. This field is deprecated!
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithPasswordMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithPasswordMethodBody")
instance ToJSON SubmitSelfServiceLoginFlowWithPasswordMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithPasswordMethodBody")


-- | 
data SubmitSelfServiceLoginFlowWithTotpMethodBody = SubmitSelfServiceLoginFlowWithTotpMethodBody
  { submitSelfServiceLoginFlowWithTotpMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ Sending the anti-csrf token is only required for browser login flows.
  , submitSelfServiceLoginFlowWithTotpMethodBodyMethod :: Text -- ^ Method should be set to \"totp\" when logging in using the TOTP strategy.
  , submitSelfServiceLoginFlowWithTotpMethodBodyTotpUnderscorecode :: Text -- ^ The TOTP code.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithTotpMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithTotpMethodBody")
instance ToJSON SubmitSelfServiceLoginFlowWithTotpMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithTotpMethodBody")


-- | 
data SubmitSelfServiceLoginFlowWithWebAuthnMethodBody = SubmitSelfServiceLoginFlowWithWebAuthnMethodBody
  { submitSelfServiceLoginFlowWithWebAuthnMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ Sending the anti-csrf token is only required for browser login flows.
  , submitSelfServiceLoginFlowWithWebAuthnMethodBodyIdentifier :: Maybe Text -- ^ Identifier is the email or username of the user trying to log in. This field is only required when using WebAuthn for passwordless login. When using WebAuthn for multi-factor authentication, it is not needed.
  , submitSelfServiceLoginFlowWithWebAuthnMethodBodyMethod :: Text -- ^ Method should be set to \"webAuthn\" when logging in using the WebAuthn strategy.
  , submitSelfServiceLoginFlowWithWebAuthnMethodBodyWebauthnUnderscorelogin :: Maybe Text -- ^ Login a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithWebAuthnMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithWebAuthnMethodBody")
instance ToJSON SubmitSelfServiceLoginFlowWithWebAuthnMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithWebAuthnMethodBody")


-- | nolint:deadcode,unused
data SubmitSelfServiceLogoutFlowWithoutBrowserBody = SubmitSelfServiceLogoutFlowWithoutBrowserBody
  { submitSelfServiceLogoutFlowWithoutBrowserBodySessionUnderscoretoken :: Text -- ^ The Session Token  Invalidate this session token.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLogoutFlowWithoutBrowserBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLogoutFlowWithoutBrowserBody")
instance ToJSON SubmitSelfServiceLogoutFlowWithoutBrowserBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLogoutFlowWithoutBrowserBody")


-- | 
data SubmitSelfServiceRecoveryFlowBody = SubmitSelfServiceRecoveryFlowBody
  { submitSelfServiceRecoveryFlowBodyCsrfUnderscoretoken :: Maybe Text -- ^ Sending the anti-csrf token is only required for browser login flows.
  , submitSelfServiceRecoveryFlowBodyEmail :: Text -- ^ Email to Recover  Needs to be set when initiating the flow. If the email is a registered recovery email, a recovery link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
  , submitSelfServiceRecoveryFlowBodyMethod :: Text -- ^ Method supports `link` only right now.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRecoveryFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRecoveryFlowBody")
instance ToJSON SubmitSelfServiceRecoveryFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRecoveryFlowBody")


-- | 
data SubmitSelfServiceRecoveryFlowWithLinkMethodBody = SubmitSelfServiceRecoveryFlowWithLinkMethodBody
  { submitSelfServiceRecoveryFlowWithLinkMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ Sending the anti-csrf token is only required for browser login flows.
  , submitSelfServiceRecoveryFlowWithLinkMethodBodyEmail :: Text -- ^ Email to Recover  Needs to be set when initiating the flow. If the email is a registered recovery email, a recovery link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
  , submitSelfServiceRecoveryFlowWithLinkMethodBodyMethod :: Text -- ^ Method supports `link` only right now.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRecoveryFlowWithLinkMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRecoveryFlowWithLinkMethodBody")
instance ToJSON SubmitSelfServiceRecoveryFlowWithLinkMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRecoveryFlowWithLinkMethodBody")


-- | 
data SubmitSelfServiceRegistrationFlowBody = SubmitSelfServiceRegistrationFlowBody
  { submitSelfServiceRegistrationFlowBodyCsrfUnderscoretoken :: Maybe Text -- ^ CSRFToken is the anti-CSRF token
  , submitSelfServiceRegistrationFlowBodyMethod :: Text -- ^ Method  Should be set to \"webauthn\" when trying to add, update, or remove a webAuthn pairing.
  , submitSelfServiceRegistrationFlowBodyPassword :: Text -- ^ Password to sign the user up with
  , submitSelfServiceRegistrationFlowBodyTraits :: Value -- ^ The identity's traits
  , submitSelfServiceRegistrationFlowBodyProvider :: Text -- ^ The provider to register with
  , submitSelfServiceRegistrationFlowBodyWebauthnUnderscoreregister :: Maybe Text -- ^ Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
  , submitSelfServiceRegistrationFlowBodyWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text -- ^ Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRegistrationFlowBody")
instance ToJSON SubmitSelfServiceRegistrationFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowBody")


-- | SubmitSelfServiceRegistrationFlowWithOidcMethodBody is used to decode the registration form payload when using the oidc method.
data SubmitSelfServiceRegistrationFlowWithOidcMethodBody = SubmitSelfServiceRegistrationFlowWithOidcMethodBody
  { submitSelfServiceRegistrationFlowWithOidcMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ The CSRF Token
  , submitSelfServiceRegistrationFlowWithOidcMethodBodyMethod :: Text -- ^ Method to use  This field must be set to `oidc` when using the oidc method.
  , submitSelfServiceRegistrationFlowWithOidcMethodBodyProvider :: Text -- ^ The provider to register with
  , submitSelfServiceRegistrationFlowWithOidcMethodBodyTraits :: Maybe Value -- ^ The identity traits
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithOidcMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRegistrationFlowWithOidcMethodBody")
instance ToJSON SubmitSelfServiceRegistrationFlowWithOidcMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowWithOidcMethodBody")


-- | SubmitSelfServiceRegistrationFlowWithPasswordMethodBody is used to decode the registration form payload when using the password method.
data SubmitSelfServiceRegistrationFlowWithPasswordMethodBody = SubmitSelfServiceRegistrationFlowWithPasswordMethodBody
  { submitSelfServiceRegistrationFlowWithPasswordMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ The CSRF Token
  , submitSelfServiceRegistrationFlowWithPasswordMethodBodyMethod :: Text -- ^ Method to use  This field must be set to `password` when using the password method.
  , submitSelfServiceRegistrationFlowWithPasswordMethodBodyPassword :: Text -- ^ Password to sign the user up with
  , submitSelfServiceRegistrationFlowWithPasswordMethodBodyTraits :: Value -- ^ The identity's traits
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithPasswordMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRegistrationFlowWithPasswordMethodBody")
instance ToJSON SubmitSelfServiceRegistrationFlowWithPasswordMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowWithPasswordMethodBody")


-- | 
data SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody = SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody
  { submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ CSRFToken is the anti-CSRF token
  , submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyMethod :: Text -- ^ Method  Should be set to \"webauthn\" when trying to add, update, or remove a webAuthn pairing.
  , submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyTraits :: Value -- ^ The identity's traits
  , submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyWebauthnUnderscoreregister :: Maybe Text -- ^ Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
  , submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text -- ^ Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRegistrationFlowWithWebAuthnMethodBody")
instance ToJSON SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowWithWebAuthnMethodBody")


-- | 
data SubmitSelfServiceSettingsFlowBody = SubmitSelfServiceSettingsFlowBody
  { submitSelfServiceSettingsFlowBodyCsrfUnderscoretoken :: Maybe Text -- ^ CSRFToken is the anti-CSRF token
  , submitSelfServiceSettingsFlowBodyMethod :: Text -- ^ Method  Should be set to \"lookup\" when trying to add, update, or remove a lookup pairing.
  , submitSelfServiceSettingsFlowBodyPassword :: Text -- ^ Password is the updated password
  , submitSelfServiceSettingsFlowBodyTraits :: Value -- ^ The identity's traits  in: body
  , submitSelfServiceSettingsFlowBodyFlow :: Maybe Text -- ^ Flow ID is the flow's ID.  in: query
  , submitSelfServiceSettingsFlowBodyLink :: Maybe Text -- ^ Link this provider  Either this or `unlink` must be set.  type: string in: body
  , submitSelfServiceSettingsFlowBodyUnlink :: Maybe Text -- ^ Unlink this provider  Either this or `link` must be set.  type: string in: body
  , submitSelfServiceSettingsFlowBodyTotpUnderscorecode :: Maybe Text -- ^ ValidationTOTP must contain a valid TOTP based on the
  , submitSelfServiceSettingsFlowBodyTotpUnderscoreunlink :: Maybe Bool -- ^ UnlinkTOTP if true will remove the TOTP pairing, effectively removing the credential. This can be used to set up a new TOTP device.
  , submitSelfServiceSettingsFlowBodyWebauthnUnderscoreregister :: Maybe Text -- ^ Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
  , submitSelfServiceSettingsFlowBodyWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text -- ^ Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
  , submitSelfServiceSettingsFlowBodyWebauthnUnderscoreremove :: Maybe Text -- ^ Remove a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
  , submitSelfServiceSettingsFlowBodyLookupUnderscoresecretUnderscoreconfirm :: Maybe Bool -- ^ If set to true will save the regenerated lookup secrets
  , submitSelfServiceSettingsFlowBodyLookupUnderscoresecretUnderscoredisable :: Maybe Bool -- ^ Disables this method if true.
  , submitSelfServiceSettingsFlowBodyLookupUnderscoresecretUnderscoreregenerate :: Maybe Bool -- ^ If set to true will regenerate the lookup secrets
  , submitSelfServiceSettingsFlowBodyLookupUnderscoresecretUnderscorereveal :: Maybe Bool -- ^ If set to true will reveal the lookup secrets
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowBody")
instance ToJSON SubmitSelfServiceSettingsFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowBody")


-- | 
data SubmitSelfServiceSettingsFlowWithLookupMethodBody = SubmitSelfServiceSettingsFlowWithLookupMethodBody
  { submitSelfServiceSettingsFlowWithLookupMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ CSRFToken is the anti-CSRF token
  , submitSelfServiceSettingsFlowWithLookupMethodBodyLookupUnderscoresecretUnderscoreconfirm :: Maybe Bool -- ^ If set to true will save the regenerated lookup secrets
  , submitSelfServiceSettingsFlowWithLookupMethodBodyLookupUnderscoresecretUnderscoredisable :: Maybe Bool -- ^ Disables this method if true.
  , submitSelfServiceSettingsFlowWithLookupMethodBodyLookupUnderscoresecretUnderscoreregenerate :: Maybe Bool -- ^ If set to true will regenerate the lookup secrets
  , submitSelfServiceSettingsFlowWithLookupMethodBodyLookupUnderscoresecretUnderscorereveal :: Maybe Bool -- ^ If set to true will reveal the lookup secrets
  , submitSelfServiceSettingsFlowWithLookupMethodBodyMethod :: Text -- ^ Method  Should be set to \"lookup\" when trying to add, update, or remove a lookup pairing.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithLookupMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithLookupMethodBody")
instance ToJSON SubmitSelfServiceSettingsFlowWithLookupMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithLookupMethodBody")


-- | nolint:deadcode,unused
data SubmitSelfServiceSettingsFlowWithOidcMethodBody = SubmitSelfServiceSettingsFlowWithOidcMethodBody
  { submitSelfServiceSettingsFlowWithOidcMethodBodyFlow :: Maybe Text -- ^ Flow ID is the flow's ID.  in: query
  , submitSelfServiceSettingsFlowWithOidcMethodBodyLink :: Maybe Text -- ^ Link this provider  Either this or `unlink` must be set.  type: string in: body
  , submitSelfServiceSettingsFlowWithOidcMethodBodyMethod :: Text -- ^ Method  Should be set to profile when trying to update a profile.
  , submitSelfServiceSettingsFlowWithOidcMethodBodyTraits :: Maybe Value -- ^ The identity's traits  in: body
  , submitSelfServiceSettingsFlowWithOidcMethodBodyUnlink :: Maybe Text -- ^ Unlink this provider  Either this or `link` must be set.  type: string in: body
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithOidcMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithOidcMethodBody")
instance ToJSON SubmitSelfServiceSettingsFlowWithOidcMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithOidcMethodBody")


-- | 
data SubmitSelfServiceSettingsFlowWithPasswordMethodBody = SubmitSelfServiceSettingsFlowWithPasswordMethodBody
  { submitSelfServiceSettingsFlowWithPasswordMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ CSRFToken is the anti-CSRF token
  , submitSelfServiceSettingsFlowWithPasswordMethodBodyMethod :: Text -- ^ Method  Should be set to password when trying to update a password.
  , submitSelfServiceSettingsFlowWithPasswordMethodBodyPassword :: Text -- ^ Password is the updated password
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithPasswordMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithPasswordMethodBody")
instance ToJSON SubmitSelfServiceSettingsFlowWithPasswordMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithPasswordMethodBody")


-- | nolint:deadcode,unused
data SubmitSelfServiceSettingsFlowWithProfileMethodBody = SubmitSelfServiceSettingsFlowWithProfileMethodBody
  { submitSelfServiceSettingsFlowWithProfileMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ The Anti-CSRF Token  This token is only required when performing browser flows.
  , submitSelfServiceSettingsFlowWithProfileMethodBodyMethod :: Text -- ^ Method  Should be set to profile when trying to update a profile.
  , submitSelfServiceSettingsFlowWithProfileMethodBodyTraits :: Value -- ^ Traits contains all of the identity's traits.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithProfileMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithProfileMethodBody")
instance ToJSON SubmitSelfServiceSettingsFlowWithProfileMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithProfileMethodBody")


-- | 
data SubmitSelfServiceSettingsFlowWithTotpMethodBody = SubmitSelfServiceSettingsFlowWithTotpMethodBody
  { submitSelfServiceSettingsFlowWithTotpMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ CSRFToken is the anti-CSRF token
  , submitSelfServiceSettingsFlowWithTotpMethodBodyMethod :: Text -- ^ Method  Should be set to \"totp\" when trying to add, update, or remove a totp pairing.
  , submitSelfServiceSettingsFlowWithTotpMethodBodyTotpUnderscorecode :: Maybe Text -- ^ ValidationTOTP must contain a valid TOTP based on the
  , submitSelfServiceSettingsFlowWithTotpMethodBodyTotpUnderscoreunlink :: Maybe Bool -- ^ UnlinkTOTP if true will remove the TOTP pairing, effectively removing the credential. This can be used to set up a new TOTP device.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithTotpMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithTotpMethodBody")
instance ToJSON SubmitSelfServiceSettingsFlowWithTotpMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithTotpMethodBody")


-- | 
data SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody = SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody
  { submitSelfServiceSettingsFlowWithWebAuthnMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ CSRFToken is the anti-CSRF token
  , submitSelfServiceSettingsFlowWithWebAuthnMethodBodyMethod :: Text -- ^ Method  Should be set to \"webauthn\" when trying to add, update, or remove a webAuthn pairing.
  , submitSelfServiceSettingsFlowWithWebAuthnMethodBodyWebauthnUnderscoreregister :: Maybe Text -- ^ Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
  , submitSelfServiceSettingsFlowWithWebAuthnMethodBodyWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text -- ^ Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
  , submitSelfServiceSettingsFlowWithWebAuthnMethodBodyWebauthnUnderscoreremove :: Maybe Text -- ^ Remove a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithWebAuthnMethodBody")
instance ToJSON SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithWebAuthnMethodBody")


-- | nolint:deadcode,unused
data SubmitSelfServiceVerificationFlowBody = SubmitSelfServiceVerificationFlowBody
  { submitSelfServiceVerificationFlowBodyCsrfUnderscoretoken :: Maybe Text -- ^ Sending the anti-csrf token is only required for browser login flows.
  , submitSelfServiceVerificationFlowBodyEmail :: Text -- ^ Email to Verify  Needs to be set when initiating the flow. If the email is a registered verification email, a verification link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
  , submitSelfServiceVerificationFlowBodyMethod :: Text -- ^ Method supports `link` only right now.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceVerificationFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceVerificationFlowBody")
instance ToJSON SubmitSelfServiceVerificationFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceVerificationFlowBody")


-- | 
data SubmitSelfServiceVerificationFlowWithLinkMethodBody = SubmitSelfServiceVerificationFlowWithLinkMethodBody
  { submitSelfServiceVerificationFlowWithLinkMethodBodyCsrfUnderscoretoken :: Maybe Text -- ^ Sending the anti-csrf token is only required for browser login flows.
  , submitSelfServiceVerificationFlowWithLinkMethodBodyEmail :: Text -- ^ Email to Verify  Needs to be set when initiating the flow. If the email is a registered verification email, a verification link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
  , submitSelfServiceVerificationFlowWithLinkMethodBodyMethod :: Text -- ^ Method supports `link` only right now.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceVerificationFlowWithLinkMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceVerificationFlowWithLinkMethodBody")
instance ToJSON SubmitSelfServiceVerificationFlowWithLinkMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceVerificationFlowWithLinkMethodBody")


-- | The Response for Login Flows via API
data SuccessfulSelfServiceLoginWithoutBrowser = SuccessfulSelfServiceLoginWithoutBrowser
  { successfulSelfServiceLoginWithoutBrowserSession :: Session -- ^ 
  , successfulSelfServiceLoginWithoutBrowserSessionUnderscoretoken :: Maybe Text -- ^ The Session Token  A session token is equivalent to a session cookie, but it can be sent in the HTTP Authorization Header:  Authorization: bearer ${session-token}  The session token is only issued for API flows, not for Browser flows!
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SuccessfulSelfServiceLoginWithoutBrowser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "successfulSelfServiceLoginWithoutBrowser")
instance ToJSON SuccessfulSelfServiceLoginWithoutBrowser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "successfulSelfServiceLoginWithoutBrowser")


-- | The Response for Registration Flows via API
data SuccessfulSelfServiceRegistrationWithoutBrowser = SuccessfulSelfServiceRegistrationWithoutBrowser
  { successfulSelfServiceRegistrationWithoutBrowserIdentity :: Identity -- ^ 
  , successfulSelfServiceRegistrationWithoutBrowserSession :: Maybe Session -- ^ 
  , successfulSelfServiceRegistrationWithoutBrowserSessionUnderscoretoken :: Maybe Text -- ^ The Session Token  This field is only set when the session hook is configured as a post-registration hook.  A session token is equivalent to a session cookie, but it can be sent in the HTTP Authorization Header:  Authorization: bearer ${session-token}  The session token is only issued for API flows, not for Browser flows!
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SuccessfulSelfServiceRegistrationWithoutBrowser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "successfulSelfServiceRegistrationWithoutBrowser")
instance ToJSON SuccessfulSelfServiceRegistrationWithoutBrowser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "successfulSelfServiceRegistrationWithoutBrowser")


-- | Container represents a HTML Form. The container can work with both HTTP Form and JSON requests
data UiContainer = UiContainer
  { uiContainerAction :: Text -- ^ Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
  , uiContainerMessages :: Maybe [UiText] -- ^ 
  , uiContainerMethod :: Text -- ^ Method is the form method (e.g. POST)
  , uiContainerNodes :: [UiNode] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiContainer where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiContainer")
instance ToJSON UiContainer where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiContainer")


-- | Nodes are represented as HTML elements or their native UI equivalents. For example, a node can be an &#x60;&lt;img&gt;&#x60; tag, or an &#x60;&lt;input element&gt;&#x60; but also &#x60;some plain text&#x60;.
data UiNode = UiNode
  { uiNodeAttributes :: UiNodeAttributes -- ^ 
  , uiNodeGroup :: Text -- ^ Group specifies which group (e.g. password authenticator) this node belongs to.
  , uiNodeMessages :: [UiText] -- ^ 
  , uiNodeMeta :: UiNodeMeta -- ^ 
  , uiNodeType :: Text -- ^ The node's type
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiNode where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNode")
instance ToJSON UiNode where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNode")


-- | 
data UiNodeAnchorAttributes = UiNodeAnchorAttributes
  { uiNodeAnchorAttributesHref :: Text -- ^ The link's href (destination) URL.  format: uri
  , uiNodeAnchorAttributesId :: Text -- ^ A unique identifier
  , uiNodeAnchorAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"a\".
  , uiNodeAnchorAttributesTitle :: UiText -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiNodeAnchorAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeAnchorAttributes")
instance ToJSON UiNodeAnchorAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeAnchorAttributes")


-- | 
data UiNodeAttributes = UiNodeAttributes
  { uiNodeAttributesDisabled :: Bool -- ^ Sets the input's disabled field to true or false.
  , uiNodeAttributesLabel :: Maybe UiText -- ^ 
  , uiNodeAttributesName :: Text -- ^ The input's element name.
  , uiNodeAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0. In this struct it technically always is \"script\".
  , uiNodeAttributesOnclick :: Maybe Text -- ^ OnClick may contain javascript which should be executed on click. This is primarily used for WebAuthn.
  , uiNodeAttributesPattern :: Maybe Text -- ^ The input's pattern.
  , uiNodeAttributesRequired :: Maybe Bool -- ^ Mark this input field as required.
  , uiNodeAttributesType :: Text -- ^ The script MIME type
  , uiNodeAttributesValue :: Maybe Value -- ^ The input's value.
  , uiNodeAttributesId :: Text -- ^ A unique identifier
  , uiNodeAttributesText :: UiText -- ^ 
  , uiNodeAttributesHeight :: Integer -- ^ Height of the image
  , uiNodeAttributesSrc :: Text -- ^ The script source
  , uiNodeAttributesWidth :: Integer -- ^ Width of the image
  , uiNodeAttributesHref :: Text -- ^ The link's href (destination) URL.  format: uri
  , uiNodeAttributesTitle :: UiText -- ^ 
  , uiNodeAttributesAsync :: Bool -- ^ The script async type
  , uiNodeAttributesCrossorigin :: Text -- ^ The script cross origin policy
  , uiNodeAttributesIntegrity :: Text -- ^ The script's integrity hash
  , uiNodeAttributesNonce :: Text -- ^ Nonce for CSP  A nonce you may want to use to improve your Content Security Policy. You do not have to use this value but if you want to improve your CSP policies you may use it. You can also choose to use your own nonce value!
  , uiNodeAttributesReferrerpolicy :: Text -- ^ The script referrer policy
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiNodeAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeAttributes")
instance ToJSON UiNodeAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeAttributes")


-- | 
data UiNodeImageAttributes = UiNodeImageAttributes
  { uiNodeImageAttributesHeight :: Integer -- ^ Height of the image
  , uiNodeImageAttributesId :: Text -- ^ A unique identifier
  , uiNodeImageAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"img\".
  , uiNodeImageAttributesSrc :: Text -- ^ The image's source URL.  format: uri
  , uiNodeImageAttributesWidth :: Integer -- ^ Width of the image
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiNodeImageAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeImageAttributes")
instance ToJSON UiNodeImageAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeImageAttributes")


-- | InputAttributes represents the attributes of an input node
data UiNodeInputAttributes = UiNodeInputAttributes
  { uiNodeInputAttributesDisabled :: Bool -- ^ Sets the input's disabled field to true or false.
  , uiNodeInputAttributesLabel :: Maybe UiText -- ^ 
  , uiNodeInputAttributesName :: Text -- ^ The input's element name.
  , uiNodeInputAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"input\".
  , uiNodeInputAttributesOnclick :: Maybe Text -- ^ OnClick may contain javascript which should be executed on click. This is primarily used for WebAuthn.
  , uiNodeInputAttributesPattern :: Maybe Text -- ^ The input's pattern.
  , uiNodeInputAttributesRequired :: Maybe Bool -- ^ Mark this input field as required.
  , uiNodeInputAttributesType :: Text -- ^ 
  , uiNodeInputAttributesValue :: Maybe Value -- ^ The input's value.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiNodeInputAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeInputAttributes")
instance ToJSON UiNodeInputAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeInputAttributes")


-- | This might include a label and other information that can optionally be used to render UIs.
data UiNodeMeta = UiNodeMeta
  { uiNodeMetaLabel :: Maybe UiText -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiNodeMeta where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeMeta")
instance ToJSON UiNodeMeta where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeMeta")


-- | 
data UiNodeScriptAttributes = UiNodeScriptAttributes
  { uiNodeScriptAttributesAsync :: Bool -- ^ The script async type
  , uiNodeScriptAttributesCrossorigin :: Text -- ^ The script cross origin policy
  , uiNodeScriptAttributesId :: Text -- ^ A unique identifier
  , uiNodeScriptAttributesIntegrity :: Text -- ^ The script's integrity hash
  , uiNodeScriptAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0. In this struct it technically always is \"script\".
  , uiNodeScriptAttributesNonce :: Text -- ^ Nonce for CSP  A nonce you may want to use to improve your Content Security Policy. You do not have to use this value but if you want to improve your CSP policies you may use it. You can also choose to use your own nonce value!
  , uiNodeScriptAttributesReferrerpolicy :: Text -- ^ The script referrer policy
  , uiNodeScriptAttributesSrc :: Text -- ^ The script source
  , uiNodeScriptAttributesType :: Text -- ^ The script MIME type
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiNodeScriptAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeScriptAttributes")
instance ToJSON UiNodeScriptAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeScriptAttributes")


-- | 
data UiNodeTextAttributes = UiNodeTextAttributes
  { uiNodeTextAttributesId :: Text -- ^ A unique identifier
  , uiNodeTextAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"text\".
  , uiNodeTextAttributesText :: UiText -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiNodeTextAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeTextAttributes")
instance ToJSON UiNodeTextAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeTextAttributes")


-- | 
data UiText = UiText
  { uiTextContext :: Maybe Value -- ^ The message's context. Useful when customizing messages.
  , uiTextId :: Integer -- ^ 
  , uiTextText :: Text -- ^ The message text. Written in american english.
  , uiTextType :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UiText where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiText")
instance ToJSON UiText where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiText")


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
  } deriving (Show, Eq, Generic, Data)

instance FromJSON VerifiableIdentityAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "verifiableIdentityAddress")
instance ToJSON VerifiableIdentityAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "verifiableIdentityAddress")


-- | 
data Version = Version
  { versionVersion :: Maybe Text -- ^ Version is the service's version.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Version where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "version")
instance ToJSON Version where
  toJSON = genericToJSON (removeFieldLabelPrefix False "version")


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do vice versa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("$", "'Dollar")
      , ("^", "'Caret")
      , ("|", "'Pipe")
      , ("=", "'Equal")
      , ("*", "'Star")
      , ("-", "'Dash")
      , ("&", "'Ampersand")
      , ("%", "'Percent")
      , ("#", "'Hash")
      , ("@", "'At")
      , ("!", "'Exclamation")
      , ("+", "'Plus")
      , (":", "'Colon")
      , (";", "'Semicolon")
      , (">", "'GreaterThan")
      , ("<", "'LessThan")
      , (".", "'Period")
      , ("_", "'Underscore")
      , ("?", "'Question_Mark")
      , (",", "'Comma")
      , ("'", "'Quote")
      , ("/", "'Slash")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("{", "'Left_Curly_Bracket")
      , ("}", "'Right_Curly_Bracket")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("~", "'Tilde")
      , ("`", "'Backtick")
      , ("<=", "'Less_Than_Or_Equal_To")
      , (">=", "'Greater_Than_Or_Equal_To")
      , ("!=", "'Not_Equal")
      , ("~=", "'Tilde_Equal")
      , ("\\", "'Back_Slash")
      , ("\"", "'Double_Quote")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace

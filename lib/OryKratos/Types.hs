{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types
  ( CompleteSelfServiceLoginFlowWithPasswordMethod (..),
    CompleteSelfServiceRecoveryFlowWithLinkMethod (..),
    CompleteSelfServiceSettingsFlowWithPasswordMethod (..),
    CompleteSelfServiceVerificationFlowWithLinkMethod (..),
    CreateIdentity (..),
    CreateRecoveryLink (..),
    ErrorContainer (..),
    FormField (..),
    GenericError (..),
    GenericErrorPayload (..),
    HealthNotReadyStatus (..),
    HealthStatus (..),
    Identity (..),
    LoginFlow (..),
    LoginFlowMethods (..),
    LoginFlowMethod (..),
    LoginFlowMethodConfig (..),
    LoginViaApiResponse (..),
    Message (..),
    RecoveryAddress (..),
    RecoveryFlow (..),
    RecoveryFlowMethods (..),
    RecoveryFlowMethod (..),
    RecoveryFlowMethodConfig (..),
    RecoveryLink (..),
    RegistrationFlow (..),
    RegistrationFlowMethods (..),
    RegistrationFlowMethod (..),
    RegistrationFlowMethodConfig (..),
    RegistrationViaApiResponse (..),
    RevokeSession (..),
    Session (..),
    SettingsFlow (..),
    SettingsFlowMethods (..),
    SettingsFlowMethod (..),
    SettingsFlowMethodConfig (..),
    SettingsViaApiResponse (..),
    UpdateIdentity (..),
    VerifiableAddress (..),
    VerificationFlow (..),
    VerificationFlowMethods (..),
    VerificationFlowMethod (..),
    VerificationFlowMethodConfig (..),
    Version (..),
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

typeFieldRename :: String -> String
typeFieldRename "_type" = "type"
typeFieldRename x = x

-- |
data CompleteSelfServiceLoginFlowWithPasswordMethod = CompleteSelfServiceLoginFlowWithPasswordMethod
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Identifier is the email or username of the user trying to log in.
    identifier :: Maybe Text,
    -- | The user's password.
    password :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CompleteSelfServiceLoginFlowWithPasswordMethod

instance ToJSON CompleteSelfServiceLoginFlowWithPasswordMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data CompleteSelfServiceRecoveryFlowWithLinkMethod = CompleteSelfServiceRecoveryFlowWithLinkMethod
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Email to Recover  Needs to be set when initiating the flow. If the email is a registered recovery email, a recovery link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email in: body
    email :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CompleteSelfServiceRecoveryFlowWithLinkMethod

instance ToJSON CompleteSelfServiceRecoveryFlowWithLinkMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data CompleteSelfServiceSettingsFlowWithPasswordMethod = CompleteSelfServiceSettingsFlowWithPasswordMethod
  { -- | CSRFToken is the anti-CSRF token  type: string
    csrf_token :: Maybe Text,
    -- | Password is the updated password  type: string
    password :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CompleteSelfServiceSettingsFlowWithPasswordMethod

instance ToJSON CompleteSelfServiceSettingsFlowWithPasswordMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data CompleteSelfServiceVerificationFlowWithLinkMethod = CompleteSelfServiceVerificationFlowWithLinkMethod
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Email to Verify  Needs to be set when initiating the flow. If the email is a registered verification email, a verification link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email in: body
    email :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CompleteSelfServiceVerificationFlowWithLinkMethod

instance ToJSON CompleteSelfServiceVerificationFlowWithLinkMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data CreateIdentity = CreateIdentity
  { -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
    schema_id :: Text,
    -- | Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_url`.
    traits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CreateIdentity

instance ToJSON CreateIdentity where
  toEncoding = genericToEncoding defaultOptions

-- |
data CreateRecoveryLink = CreateRecoveryLink
  { -- | Link Expires In  The recovery link will expire at that point in time. Defaults to the configuration value of `selfservice.flows.recovery.request_lifespan`.
    expires_in :: Maybe Text,
    -- |
    identity_id :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CreateRecoveryLink

instance ToJSON CreateRecoveryLink where
  toEncoding = genericToEncoding defaultOptions

-- |
data ErrorContainer = ErrorContainer
  { -- | Errors in the container
    errors :: Value,
    -- |
    id :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ErrorContainer

instance ToJSON ErrorContainer where
  toEncoding = genericToEncoding defaultOptions

-- | Field represents a HTML Form Field
data FormField = FormField
  { -- | Disabled is the equivalent of `<input {{if .Disabled}}disabled{{end}}\">`
    disabled :: Maybe Bool,
    -- |
    messages :: Maybe [Message],
    -- | Name is the equivalent of `<input name=\"{{.Name}}\">`
    name :: Text,
    -- | Pattern is the equivalent of `<input pattern=\"{{.Pattern}}\">`
    pattern :: Maybe Text,
    -- | Required is the equivalent of `<input required=\"{{.Required}}\">`
    required :: Maybe Bool,
    -- | Type is the equivalent of `<input type=\"{{.Type}}\">`
    _type :: Text,
    -- | Value is the equivalent of `<input value=\"{{.Value}}\">`
    value :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON FormField where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON FormField where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- | Error responses are sent when an error (e.g. unauthorized, bad request, ...) occurred.
data GenericError = GenericError
  { -- |
    error :: Maybe GenericErrorPayload
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON GenericError

instance ToJSON GenericError where
  toEncoding = genericToEncoding defaultOptions

-- |
data GenericErrorPayload = GenericErrorPayload
  { -- | Code represents the error status code (404, 403, 401, ...).
    code :: Maybe Integer,
    -- | Debug contains debug information. This is usually not available and has to be enabled.
    debug :: Maybe Text,
    -- |
    details :: Maybe Value,
    -- |
    message :: Maybe Text,
    -- |
    reason :: Maybe Text,
    -- |
    request :: Maybe Text,
    -- |
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON GenericErrorPayload

instance ToJSON GenericErrorPayload where
  toEncoding = genericToEncoding defaultOptions

-- |
data HealthNotReadyStatus = HealthNotReadyStatus
  { -- | Errors contains a list of errors that caused the not ready status.
    errors :: Maybe (Map.Map String Text)
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON HealthNotReadyStatus

instance ToJSON HealthNotReadyStatus where
  toEncoding = genericToEncoding defaultOptions

-- |
data HealthStatus = HealthStatus
  { -- | Status always contains \"ok\".
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON HealthStatus

instance ToJSON HealthStatus where
  toEncoding = genericToEncoding defaultOptions

-- |
data Identity = Identity
  { -- |
    id :: Text,
    -- | RecoveryAddresses contains all the addresses that can be used to recover an identity.
    recovery_addresses :: Maybe [RecoveryAddress],
    -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
    schema_id :: Text,
    -- | SchemaURL is the URL of the endpoint where the identity's traits schema can be fetched from.  format: url
    schema_url :: Text,
    -- |
    traits :: Value,
    -- | VerifiableAddresses contains all the addresses that can be verified by the user.
    verifiable_addresses :: Maybe [VerifiableAddress]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Identity

instance ToJSON Identity where
  toEncoding = genericToEncoding defaultOptions

-- | This object represents a login flow. A login flow is initiated at the \&quot;Initiate Login API / Browser Flow\&quot; endpoint by a client.  Once a login flow is completed successfully, a session cookie or session token will be issued.
data LoginFlow = LoginFlow
  { -- | and so on.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to log in, a new flow has to be initiated.
    expires_at :: UTCTime,
    -- | Forced stores whether this login flow should enforce re-authentication.
    forced :: Maybe Bool,
    -- |
    id :: Text,
    -- | IssuedAt is the time (UTC) when the flow started.
    issued_at :: UTCTime,
    -- |
    messages :: Maybe [Message],
    -- | List of login methods  This is the list of available login methods with their required form fields, such as `identifier` and `password` for the password login method. This will also contain error messages such as \"password can not be empty\".
    methods :: LoginFlowMethods,
    -- | RequestURL is the initial URL that was requested from ORY Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON LoginFlow where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON LoginFlow where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

data LoginFlowMethods = LoginFlowMethods
  {
    password :: Maybe LoginFlowMethod,
    oidc :: Maybe LoginFlowMethod
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON LoginFlowMethods

instance ToJSON LoginFlowMethods where
  toEncoding = genericToEncoding defaultOptions

-- |
data LoginFlowMethod = LoginFlowMethod
  { -- |
    config :: LoginFlowMethodConfig,
    -- | and so on.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON LoginFlowMethod

instance ToJSON LoginFlowMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data LoginFlowMethodConfig = LoginFlowMethodConfig
  { -- | Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
    action :: Text,
    -- | Fields contains multiple fields
    fields :: [FormField],
    -- |
    messages :: Maybe [Message],
    -- | Method is the form method (e.g. POST)
    method :: Text,
    -- | Providers is set for the \"oidc\" flow method.
    providers :: Maybe [FormField]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON LoginFlowMethodConfig

instance ToJSON LoginFlowMethodConfig where
  toEncoding = genericToEncoding defaultOptions

-- | The Response for Login Flows via API
data LoginViaApiResponse = LoginViaApiResponse
  { -- |
    session :: Session,
    -- | The Session Token  A session token is equivalent to a session cookie, but it can be sent in the HTTP Authorization Header:  Authorization: bearer ${session-token}  The session token is only issued for API flows, not for Browser flows!
    session_token :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON LoginViaApiResponse

instance ToJSON LoginViaApiResponse where
  toEncoding = genericToEncoding defaultOptions

-- |
data Message = Message
  { -- |
    context :: Maybe Value,
    -- |
    id :: Maybe Integer,
    -- |
    text :: Maybe Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Message where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON Message where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- |
data RecoveryAddress = RecoveryAddress
  { -- |
    id :: Text,
    -- |
    value :: Text,
    -- |
    via :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RecoveryAddress

instance ToJSON RecoveryAddress where
  toEncoding = genericToEncoding defaultOptions

-- | This request is used when an identity wants to recover their account.  We recommend reading the [Account Recovery Documentation](../self-service/flows/password-reset-account-recovery)
data RecoveryFlow = RecoveryFlow
  { -- | Active, if set, contains the registration method that is being used. It is initially not set.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the request expires. If the user still wishes to update the setting, a new request has to be initiated.
    expires_at :: UTCTime,
    -- |
    id :: Text,
    -- | IssuedAt is the time (UTC) when the request occurred.
    issued_at :: UTCTime,
    -- |
    messages :: Maybe [Message],
    -- | Methods contains context for all account recovery methods. If a registration request has been processed, but for example the password is incorrect, this will contain error messages.
    methods :: RecoveryFlowMethods,
    -- | RequestURL is the initial URL that was requested from ORY Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- |
    state :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RecoveryFlow where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON RecoveryFlow where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- |
data RecoveryFlowMethods = RecoveryFlowMethods
  { -- |
    link :: Maybe RecoveryFlowMethod
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RecoveryFlowMethods

instance ToJSON RecoveryFlowMethods where
  toEncoding = genericToEncoding defaultOptions

-- |
data RecoveryFlowMethod = RecoveryFlowMethod
  { -- |
    config :: RecoveryFlowMethodConfig,
    -- | Method contains the request credentials type.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RecoveryFlowMethod

instance ToJSON RecoveryFlowMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data RecoveryFlowMethodConfig = RecoveryFlowMethodConfig
  { -- | Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
    action :: Text,
    -- | Fields contains multiple fields
    fields :: [FormField],
    -- |
    messages :: Maybe [Message],
    -- | Method is the form method (e.g. POST)
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RecoveryFlowMethodConfig

instance ToJSON RecoveryFlowMethodConfig where
  toEncoding = genericToEncoding defaultOptions

-- |
data RecoveryLink = RecoveryLink
  { -- | Recovery Link Expires At  The timestamp when the recovery link expires.
    expires_at :: Maybe UTCTime,
    -- | Recovery Link  This link can be used to recover the account.
    recovery_link :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RecoveryLink

instance ToJSON RecoveryLink where
  toEncoding = genericToEncoding defaultOptions

-- |
data RegistrationFlow = RegistrationFlow
  { -- | and so on.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to log in, a new flow has to be initiated.
    expires_at :: UTCTime,
    -- |
    id :: Text,
    -- | IssuedAt is the time (UTC) when the flow occurred.
    issued_at :: UTCTime,
    -- |
    messages :: Maybe [Message],
    -- | Methods contains context for all enabled registration methods. If a registration flow has been processed, but for example the password is incorrect, this will contain error messages.
    methods :: RegistrationFlowMethods,
    -- | RequestURL is the initial URL that was requested from ORY Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RegistrationFlow where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON RegistrationFlow where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- |
data RegistrationFlowMethods = RegistrationFlowMethods
  { -- |
    password :: Maybe RegistrationFlowMethod,
    -- | and so on.
    oidc :: Maybe RegistrationFlowMethod
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RegistrationFlowMethods

instance ToJSON RegistrationFlowMethods where
  toEncoding = genericToEncoding defaultOptions

-- |
data RegistrationFlowMethod = RegistrationFlowMethod
  { -- |
    config :: RegistrationFlowMethodConfig,
    -- | and so on.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RegistrationFlowMethod

instance ToJSON RegistrationFlowMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data RegistrationFlowMethodConfig = RegistrationFlowMethodConfig
  { -- | Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
    action :: Text,
    -- | Fields contains multiple fields
    fields :: [FormField],
    -- |
    messages :: Maybe [Message],
    -- | Method is the form method (e.g. POST)
    method :: Text,
    -- | Providers is set for the \"oidc\" registration method.
    providers :: Maybe [FormField]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RegistrationFlowMethodConfig

instance ToJSON RegistrationFlowMethodConfig where
  toEncoding = genericToEncoding defaultOptions

-- | The Response for Registration Flows via API
data RegistrationViaApiResponse = RegistrationViaApiResponse
  { -- |
    identity :: Identity,
    -- |
    session :: Maybe Session,
    -- | The Session Token  This field is only set when the session hook is configured as a post-registration hook.  A session token is equivalent to a session cookie, but it can be sent in the HTTP Authorization Header:  Authorization: bearer ${session-token}  The session token is only issued for API flows, not for Browser flows!
    session_token :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RegistrationViaApiResponse

instance ToJSON RegistrationViaApiResponse where
  toEncoding = genericToEncoding defaultOptions

-- |
data RevokeSession = RevokeSession
  { -- | The Session Token  Invalidate this session token.
    session_token :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RevokeSession

instance ToJSON RevokeSession where
  toEncoding = genericToEncoding defaultOptions

-- |
data Session = Session
  { -- |
    active :: Maybe Bool,
    -- |
    authenticated_at :: UTCTime,
    -- |
    expires_at :: UTCTime,
    -- |
    id :: Text,
    -- |
    identity :: Identity,
    -- |
    issued_at :: UTCTime
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Session

instance ToJSON Session where
  toEncoding = genericToEncoding defaultOptions

-- | This flow is used when an identity wants to update settings (e.g. profile data, passwords, ...) in a selfservice manner.  We recommend reading the [User Settings Documentation](../self-service/flows/user-settings)
data SettingsFlow = SettingsFlow
  { -- | Active, if set, contains the registration method that is being used. It is initially not set.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to update the setting, a new flow has to be initiated.
    expires_at :: UTCTime,
    -- |
    id :: Text,
    -- |
    identity :: Identity,
    -- | IssuedAt is the time (UTC) when the flow occurred.
    issued_at :: UTCTime,
    -- |
    messages :: Maybe [Message],
    -- | Methods contains context for all enabled registration methods. If a settings flow has been processed, but for example the first name is empty, this will contain error messages.
    methods :: SettingsFlowMethods,
    -- | RequestURL is the initial URL that was requested from ORY Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- |
    state :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SettingsFlow where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON SettingsFlow where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- |
data SettingsFlowMethods = SettingsFlowMethods
  { -- |
    profile :: Maybe SettingsFlowMethod,
    password :: Maybe SettingsFlowMethod,
    oidc :: Maybe SettingsFlowMethod
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SettingsFlowMethods

instance ToJSON SettingsFlowMethods where
  toEncoding = genericToEncoding defaultOptions

-- |
data SettingsFlowMethod = SettingsFlowMethod
  { -- |
    config :: SettingsFlowMethodConfig,
    -- | Method is the name of this flow method.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SettingsFlowMethod

instance ToJSON SettingsFlowMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data SettingsFlowMethodConfig = SettingsFlowMethodConfig
  { -- | Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
    action :: Text,
    -- | Fields contains multiple fields
    fields :: [FormField],
    -- |
    messages :: Maybe [Message],
    -- | Method is the form method (e.g. POST)
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SettingsFlowMethodConfig

instance ToJSON SettingsFlowMethodConfig where
  toEncoding = genericToEncoding defaultOptions

-- | The Response for Settings Flows via API
data SettingsViaApiResponse = SettingsViaApiResponse
  { -- |
    flow :: SettingsFlow,
    -- |
    identity :: Identity
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SettingsViaApiResponse

instance ToJSON SettingsViaApiResponse where
  toEncoding = genericToEncoding defaultOptions

-- |
data UpdateIdentity = UpdateIdentity
  { -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits. If set will update the Identity's SchemaID.
    schema_id :: Maybe Text,
    -- | Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_id`.
    traits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UpdateIdentity

instance ToJSON UpdateIdentity where
  toEncoding = genericToEncoding defaultOptions

-- |
data VerifiableAddress = VerifiableAddress
  { -- |
    id :: Text,
    -- |
    status :: Text,
    -- |
    value :: Text,
    -- |
    verified :: Bool,
    -- |
    verified_at :: Maybe UTCTime,
    -- |
    via :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerifiableAddress

instance ToJSON VerifiableAddress where
  toEncoding = genericToEncoding defaultOptions

-- | Used to verify an out-of-band communication channel such as an email address or a phone number.  For more information head over to: https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation
data VerificationFlow = VerificationFlow
  { -- | Active, if set, contains the registration method that is being used. It is initially not set.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the request expires. If the user still wishes to verify the address, a new request has to be initiated.
    expires_at :: Maybe UTCTime,
    -- |
    id :: Maybe Text,
    -- | IssuedAt is the time (UTC) when the request occurred.
    issued_at :: Maybe UTCTime,
    -- |
    messages :: Maybe [Message],
    -- | Methods contains context for all account verification methods. If a registration request has been processed, but for example the password is incorrect, this will contain error messages.
    methods :: Map.Map String VerificationFlowMethod,
    -- | RequestURL is the initial URL that was requested from ORY Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Maybe Text,
    -- |
    state :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerificationFlow where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON VerificationFlow where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- |
data VerificationFlowMethods = VerificationFlowMethods
  { -- |
    link :: Maybe VerificationFlowMethod
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerificationFlowMethods

instance ToJSON VerificationFlowMethods where
  toEncoding = genericToEncoding defaultOptions

-- |
data VerificationFlowMethod = VerificationFlowMethod
  { -- |
    config :: VerificationFlowMethodConfig,
    -- | Method contains the request credentials type.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerificationFlowMethod

instance ToJSON VerificationFlowMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data VerificationFlowMethodConfig = VerificationFlowMethodConfig
  { -- | Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
    action :: Text,
    -- | Fields contains multiple fields
    fields :: [FormField],
    -- |
    messages :: Maybe [Message],
    -- | Method is the form method (e.g. POST)
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerificationFlowMethodConfig

instance ToJSON VerificationFlowMethodConfig where
  toEncoding = genericToEncoding defaultOptions

-- |
data Version = Version
  { -- | Version is the service's version.
    version :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Version

instance ToJSON Version where
  toEncoding = genericToEncoding defaultOptions

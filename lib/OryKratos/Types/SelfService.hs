{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types.SelfService
  ( SelfServiceBrowserLocationChangeRequiredError (..),
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
import OryKratos.Types.Helper (customOptions, removeFieldLabelPrefix)
import OryKratos.Types.Identity (Identity, IdentityCredentialsType)
import OryKratos.Types.Types (AuthenticatorAssuranceLevel)
import OryKratos.Types.Ui (UiContainer)

data SelfServiceBrowserLocationChangeRequiredError = SelfServiceBrowserLocationChangeRequiredError
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
    -- | Since when the flow has expired
    redirect_browser_to :: Maybe Text,
    -- | The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
    request :: Maybe Text,
    -- | The status description
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceBrowserLocationChangeRequiredError

instance ToJSON SelfServiceBrowserLocationChangeRequiredError where
  toEncoding = genericToEncoding defaultOptions

data SelfServiceError = SelfServiceError
  { -- | CreatedAt is a helper struct field for gobuffalo.pop.
    created_at :: Maybe UTCTime,
    error :: Maybe Value,
    id :: UUID,
    -- | UpdatedAt is a helper struct field for gobuffalo.pop.
    updated_at :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceError

instance ToJSON SelfServiceError where
  toEncoding = genericToEncoding defaultOptions

-- | Is sent when a flow is expired
data SelfServiceFlowExpiredError = SelfServiceFlowExpiredError
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
    -- | The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
    request :: Maybe Text,
    -- | A Duration represents the elapsed time between two instants as an int64 nanosecond count. The representation limits the largest representable duration to approximately 290 years.
    since :: Maybe Integer,
    -- | The status description
    status :: Maybe Text,
    use_flow_id :: Maybe UUID
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceFlowExpiredError

instance ToJSON SelfServiceFlowExpiredError where
  toEncoding = genericToEncoding defaultOptions

-- | This object represents a login flow. A login flow is initiated at the \&quot;Initiate Login API / Browser Flow\&quot; endpoint by a client.  Once a login flow is completed successfully, a session cookie or session token will be issued.
data SelfServiceLoginFlow = SelfServiceLoginFlow
  { active :: Maybe IdentityCredentialsType,
    -- | CreatedAt is a helper struct field for gobuffalo.pop.
    created_at :: Maybe UTCTime,
    -- | ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to log in, a new flow has to be initiated.
    expires_at :: UTCTime,
    id :: UUID,
    -- | IssuedAt is the time (UTC) when the flow started.
    issued_at :: UTCTime,
    -- | Refresh stores whether this login flow should enforce re-authentication.
    refresh :: Maybe Bool,
    -- | RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    requested_aal :: Maybe AuthenticatorAssuranceLevel,
    -- | ReturnTo contains the requested return_to URL.
    return_to :: Maybe Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Text,
    ui :: UiContainer,
    -- | UpdatedAt is a helper struct field for gobuffalo.pop.
    updated_at :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceLoginFlow where
  parseJSON = genericParseJSON customOptions

instance ToJSON SelfServiceLoginFlow where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data SelfServiceLogoutUrl = SelfServiceLogoutUrl
  { -- | LogoutToken can be used to perform logout using AJAX.
    logout_token :: Text,
    -- | LogoutURL can be opened in a browser to sign the user out.  format: uri
    logout_url :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceLogoutUrl

instance ToJSON SelfServiceLogoutUrl where
  toEncoding = genericToEncoding defaultOptions

-- | This request is used when an identity wants to recover their account.  We recommend reading the [Account Recovery Documentation](../self-service/flows/password-reset-account-recovery)
data SelfServiceRecoveryFlow = SelfServiceRecoveryFlow
  { -- | Active, if set, contains the registration method that is being used. It is initially not set.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the request expires. If the user still wishes to update the setting, a new request has to be initiated.
    expires_at :: UTCTime,
    id :: UUID,
    -- | IssuedAt is the time (UTC) when the request occurred.
    issued_at :: UTCTime,
    -- | RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- | ReturnTo contains the requested return_to URL.
    return_to :: Maybe Text,
    state :: SelfServiceRecoveryFlowState,
    -- | The flow type can either be `api` or `browser`.
    _type :: Text,
    ui :: UiContainer
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRecoveryFlow where
  parseJSON = genericParseJSON customOptions

instance ToJSON SelfServiceRecoveryFlow where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

-- | The state represents the state of the recovery flow.  choose_method: ask the user to choose a method (e.g. recover account via email) sent_email: the email has been sent to the user passed_challenge: the request was successful and the recovery challenge was passed.
data SelfServiceRecoveryFlowState
  = SelfServiceRecoveryFlowStateChooseMethod
  | SelfServiceRecoveryFlowStateSentEmail
  | SelfServiceRecoveryFlowStatePassedChallenge
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRecoveryFlowState where
  parseJSON (Aeson.String s) = case T.unpack s of
    "choose_method" -> return SelfServiceRecoveryFlowStateChooseMethod
    "sent_email" -> return SelfServiceRecoveryFlowStateSentEmail
    "passed_challenge" -> return SelfServiceRecoveryFlowStatePassedChallenge
    _ -> Prelude.error "Invalid SelfServiceRecoveryFlowState"
  parseJSON _ = Prelude.error "Invalid SelfServiceRecoveryFlowState"

instance ToJSON SelfServiceRecoveryFlowState where
  toJSON (SelfServiceRecoveryFlowStateChooseMethod) = Aeson.String "choose_method"
  toJSON (SelfServiceRecoveryFlowStateSentEmail) = Aeson.String "sent_email"
  toJSON (SelfServiceRecoveryFlowStatePassedChallenge) = Aeson.String "passed_challenge"

data SelfServiceRecoveryLink = SelfServiceRecoveryLink
  { -- | Recovery Link Expires At  The timestamp when the recovery link expires.
    expires_at :: Maybe UTCTime,
    -- | Recovery Link  This link can be used to recover the account.
    recovery_link :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRecoveryLink

instance ToJSON SelfServiceRecoveryLink where
  toEncoding = genericToEncoding defaultOptions

data SelfServiceRegistrationFlow = SelfServiceRegistrationFlow
  { active :: Maybe IdentityCredentialsType,
    -- | ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to log in, a new flow has to be initiated.
    expires_at :: UTCTime,
    id :: UUID,
    -- | IssuedAt is the time (UTC) when the flow occurred.
    issued_at :: UTCTime,
    -- | RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- | ReturnTo contains the requested return_to URL.
    return_to :: Maybe Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Text,
    ui :: UiContainer
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRegistrationFlow where
  parseJSON = genericParseJSON customOptions

instance ToJSON SelfServiceRegistrationFlow where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

-- | This flow is used when an identity wants to update settings (e.g. profile data, passwords, ...) in a selfservice manner.  We recommend reading the [User Settings Documentation](../self-service/flows/user-settings)
data SelfServiceSettingsFlow traits metadataAdmin metadataPublic = SelfServiceSettingsFlow
  { -- | Active, if set, contains the registration method that is being used. It is initially not set.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to update the setting, a new flow has to be initiated.
    expires_at :: UTCTime,
    id :: UUID,
    identity :: Identity traits metadataAdmin metadataPublic,
    -- | IssuedAt is the time (UTC) when the flow occurred.
    issued_at :: UTCTime,
    -- | RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- | ReturnTo contains the requested return_to URL.
    return_to :: Maybe Text,
    state :: SelfServiceSettingsFlowState,
    -- | The flow type can either be `api` or `browser`.
    _type :: Text,
    ui :: UiContainer
  }
  deriving stock (Show, Eq, Generic, Data)

instance
  ( FromJSON traits,
    FromJSON metadataAdmin,
    FromJSON metadataPublic
  ) =>
  FromJSON (SelfServiceSettingsFlow traits metadataAdmin metadataPublic)
  where
  parseJSON = genericParseJSON customOptions

instance
  ( ToJSON traits,
    ToJSON metadataAdmin,
    ToJSON metadataPublic
  ) =>
  ToJSON (SelfServiceSettingsFlow traits metadataAdmin metadataPublic)
  where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

-- | show_form: No user data has been collected, or it is invalid, and thus the form should be shown. success: Indicates that the settings flow has been updated successfully with the provided data. Done will stay true when repeatedly checking. If set to true, done will revert back to false only when a flow with invalid (e.g. \&quot;please use a valid phone number\&quot;) data was sent.
data SelfServiceSettingsFlowState = SelfServiceSettingsFlowStateShowForm | SelfServiceSettingsFlowStateSuccess deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceSettingsFlowState where
  parseJSON (Aeson.String s) = case T.unpack s of
    "show_form" -> return SelfServiceSettingsFlowStateShowForm
    "success" -> return SelfServiceSettingsFlowStateSuccess
    _ -> Prelude.error "Invalid SelfServiceSettingsFlowState"
  parseJSON _ = Prelude.error "Invalid SelfServiceSettingsFlowState"

instance ToJSON SelfServiceSettingsFlowState where
  toJSON (SelfServiceSettingsFlowStateShowForm) = Aeson.String "show_form"
  toJSON (SelfServiceSettingsFlowStateSuccess) = Aeson.String "success"

-- | Used to verify an out-of-band communication channel such as an email address or a phone number.  For more information head over to: https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation
data SelfServiceVerificationFlow = SelfServiceVerificationFlow
  { -- | Active, if set, contains the registration method that is being used. It is initially not set.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the request expires. If the user still wishes to verify the address, a new request has to be initiated.
    expires_at :: Maybe UTCTime,
    id :: UUID,
    -- | IssuedAt is the time (UTC) when the request occurred.
    issued_at :: Maybe UTCTime,
    -- | RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Maybe Text,
    -- | ReturnTo contains the requested return_to URL.
    return_to :: Maybe Text,
    state :: SelfServiceVerificationFlowState,
    -- | The flow type can either be `api` or `browser`.
    _type :: Text,
    ui :: UiContainer
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceVerificationFlow where
  parseJSON = genericParseJSON customOptions

instance ToJSON SelfServiceVerificationFlow where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

-- | The state represents the state of the verification flow.  choose_method: ask the user to choose a method (e.g. recover account via email) sent_email: the email has been sent to the user passed_challenge: the request was successful and the recovery challenge was passed.
data SelfServiceVerificationFlowState
  = SelfServiceVerificationFlowStateChooseMethod
  | SelfServiceVerificationFlowStateSentEmail
  | SelfServiceVerificationFlowStatePassedChallenge
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceVerificationFlowState where
  parseJSON (Aeson.String s) = case T.unpack s of
    "choose_method" -> return SelfServiceVerificationFlowStateChooseMethod
    "sent_email" -> return SelfServiceVerificationFlowStateSentEmail
    "passed_challenge" -> return SelfServiceVerificationFlowStatePassedChallenge
    _ -> Prelude.error "Invalid SelfServiceRecoveryFlowState"
  parseJSON _ = Prelude.error "Invalid SelfServiceRecoveryFlowState"

instance ToJSON SelfServiceVerificationFlowState where
  toJSON (SelfServiceVerificationFlowStateChooseMethod) = Aeson.String "choose_method"
  toJSON (SelfServiceVerificationFlowStateSentEmail) = Aeson.String "sent_email"
  toJSON (SelfServiceVerificationFlowStatePassedChallenge) = Aeson.String "passed_challenge"

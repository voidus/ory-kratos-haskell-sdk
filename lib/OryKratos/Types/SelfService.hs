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
import OryKratos.Types.Identity (IdentityCredentialsType, Identity)
import OryKratos.Types.Types ( AuthenticatorAssuranceLevel )
import OryKratos.Types.Ui (UiContainer)
import OryKratos.Types.Helper ( removeFieldLabelPrefix )

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
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceBrowserLocationChangeRequiredError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceBrowserLocationChangeRequiredError")
instance ToJSON SelfServiceBrowserLocationChangeRequiredError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceBrowserLocationChangeRequiredError")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceBrowserLocationChangeRequiredError")


-- | 
data SelfServiceError = SelfServiceError
  { selfServiceErrorCreatedUnderscoreat :: Maybe UTCTime -- ^ CreatedAt is a helper struct field for gobuffalo.pop.
  , selfServiceErrorError :: Maybe Value -- ^ 
  , selfServiceErrorId :: Text -- ^ 
  , selfServiceErrorUpdatedUnderscoreat :: Maybe UTCTime -- ^ UpdatedAt is a helper struct field for gobuffalo.pop.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceError")
instance ToJSON SelfServiceError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceError")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceError")


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
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceFlowExpiredError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceFlowExpiredError")
instance ToJSON SelfServiceFlowExpiredError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceFlowExpiredError")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceFlowExpiredError")


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
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceLoginFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceLoginFlow")
instance ToJSON SelfServiceLoginFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceLoginFlow")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceLoginFlow")


-- | 
data SelfServiceLogoutUrl = SelfServiceLogoutUrl
  { selfServiceLogoutUrlLogoutUnderscoretoken :: Text -- ^ LogoutToken can be used to perform logout using AJAX.
  , selfServiceLogoutUrlLogoutUnderscoreurl :: Text -- ^ LogoutURL can be opened in a browser to sign the user out.  format: uri
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceLogoutUrl where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceLogoutUrl")
instance ToJSON SelfServiceLogoutUrl where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceLogoutUrl")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceLogoutUrl")


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
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRecoveryFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceRecoveryFlow")
instance ToJSON SelfServiceRecoveryFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceRecoveryFlow")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceRecoveryFlow")


-- | The state represents the state of the recovery flow.  choose_method: ask the user to choose a method (e.g. recover account via email) sent_email: the email has been sent to the user passed_challenge: the request was successful and the recovery challenge was passed.
data SelfServiceRecoveryFlowState = SelfServiceRecoveryFlowState
  { 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRecoveryFlowState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceRecoveryFlowState")
instance ToJSON SelfServiceRecoveryFlowState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceRecoveryFlowState")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceRecoveryFlowState")


-- | 
data SelfServiceRecoveryLink = SelfServiceRecoveryLink
  { selfServiceRecoveryLinkExpiresUnderscoreat :: Maybe UTCTime -- ^ Recovery Link Expires At  The timestamp when the recovery link expires.
  , selfServiceRecoveryLinkRecoveryUnderscorelink :: Text -- ^ Recovery Link  This link can be used to recover the account.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRecoveryLink where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceRecoveryLink")
instance ToJSON SelfServiceRecoveryLink where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceRecoveryLink")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceRecoveryLink")


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
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceRegistrationFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceRegistrationFlow")
instance ToJSON SelfServiceRegistrationFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceRegistrationFlow")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceRegistrationFlow")


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
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceSettingsFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceSettingsFlow")
instance ToJSON SelfServiceSettingsFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceSettingsFlow")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceSettingsFlow")


-- | show_form: No user data has been collected, or it is invalid, and thus the form should be shown. success: Indicates that the settings flow has been updated successfully with the provided data. Done will stay true when repeatedly checking. If set to true, done will revert back to false only when a flow with invalid (e.g. \&quot;please use a valid phone number\&quot;) data was sent.
data SelfServiceSettingsFlowState = SelfServiceSettingsFlowState
  { 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceSettingsFlowState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceSettingsFlowState")
instance ToJSON SelfServiceSettingsFlowState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceSettingsFlowState")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceSettingsFlowState")


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
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceVerificationFlow where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceVerificationFlow")
instance ToJSON SelfServiceVerificationFlow where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceVerificationFlow")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceVerificationFlow")


-- | The state represents the state of the verification flow.  choose_method: ask the user to choose a method (e.g. recover account via email) sent_email: the email has been sent to the user passed_challenge: the request was successful and the recovery challenge was passed.
data SelfServiceVerificationFlowState = SelfServiceVerificationFlowState
  { 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceVerificationFlowState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selfServiceVerificationFlowState")
instance ToJSON SelfServiceVerificationFlowState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selfServiceVerificationFlowState")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "selfServiceVerificationFlowState")

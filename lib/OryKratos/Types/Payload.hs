{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types.Payload
  ( SubmitSelfServiceLoginFlowBody (..),
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

data SubmitSelfServiceLoginFlowBody = SubmitSelfServiceLoginFlowBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Identifier is the email or username of the user trying to log in. This field is only required when using WebAuthn for passwordless login. When using WebAuthn for multi-factor authentication, it is not needed.
    identifier :: Text,
    -- | Method should be set to \"lookup_secret\" when logging in using the lookup_secret strategy.
    method :: Text,
    -- | The user's password.
    password :: Text,
    -- | Identifier is the email or username of the user trying to log in. This field is deprecated!
    password_identifier :: Maybe Text,
    -- | The provider to register with
    provider :: Text,
    -- | The identity traits. This is a placeholder for the registration flow.
    traits :: Maybe Value,
    -- | The TOTP code.
    totp_code :: Text,
    -- | Login a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
    webauthn_login :: Maybe Text,
    -- | The lookup secret.
    lookup_secret :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowBody

instance ToJSON SubmitSelfServiceLoginFlowBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceLoginFlowWithLookupSecretMethodBody = SubmitSelfServiceLoginFlowWithLookupSecretMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | The lookup secret.
    lookup_secret :: Text,
    -- | Method should be set to \"lookup_secret\" when logging in using the lookup_secret strategy.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithLookupSecretMethodBody

instance ToJSON SubmitSelfServiceLoginFlowWithLookupSecretMethodBody where
  toEncoding = genericToEncoding defaultOptions

-- | SubmitSelfServiceLoginFlowWithOidcMethodBody is used to decode the login form payload when using the oidc method.
data SubmitSelfServiceLoginFlowWithOidcMethodBody = SubmitSelfServiceLoginFlowWithOidcMethodBody
  { -- | The CSRF Token
    csrf_token :: Maybe Text,
    -- | Method to use  This field must be set to `oidc` when using the oidc method.
    method :: Text,
    -- | The provider to register with
    provider :: Text,
    -- | The identity traits. This is a placeholder for the registration flow.
    traits :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithOidcMethodBody

instance ToJSON SubmitSelfServiceLoginFlowWithOidcMethodBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceLoginFlowWithPasswordMethodBody = SubmitSelfServiceLoginFlowWithPasswordMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Identifier is the email or username of the user trying to log in.
    identifier :: Text,
    -- | Method should be set to \"password\" when logging in using the identifier and password strategy.
    method :: Text,
    -- | The user's password.
    password :: Text,
    -- | Identifier is the email or username of the user trying to log in. This field is deprecated!
    password_identifier :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithPasswordMethodBody

instance ToJSON SubmitSelfServiceLoginFlowWithPasswordMethodBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceLoginFlowWithTotpMethodBody = SubmitSelfServiceLoginFlowWithTotpMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Method should be set to \"totp\" when logging in using the TOTP strategy.
    method :: Text,
    -- | The TOTP code.
    totp_code :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithTotpMethodBody

instance ToJSON SubmitSelfServiceLoginFlowWithTotpMethodBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceLoginFlowWithWebAuthnMethodBody = SubmitSelfServiceLoginFlowWithWebAuthnMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Identifier is the email or username of the user trying to log in. This field is only required when using WebAuthn for passwordless login. When using WebAuthn for multi-factor authentication, it is not needed.
    identifier :: Maybe Text,
    -- | Method should be set to \"webAuthn\" when logging in using the WebAuthn strategy.
    method :: Text,
    -- | Login a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
    webauthn_login :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithWebAuthnMethodBody

instance ToJSON SubmitSelfServiceLoginFlowWithWebAuthnMethodBody where
  toEncoding = genericToEncoding defaultOptions

-- | nolint:deadcode,unused
data SubmitSelfServiceLogoutFlowWithoutBrowserBody = SubmitSelfServiceLogoutFlowWithoutBrowserBody
  { -- | The Session Token  Invalidate this session token.
    session_token :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLogoutFlowWithoutBrowserBody

instance ToJSON SubmitSelfServiceLogoutFlowWithoutBrowserBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceRecoveryFlowBody = SubmitSelfServiceRecoveryFlowBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Email to Recover  Needs to be set when initiating the flow. If the email is a registered recovery email, a recovery link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
    email :: Text,
    -- | Method supports `link` only right now.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRecoveryFlowBody

instance ToJSON SubmitSelfServiceRecoveryFlowBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceRecoveryFlowWithLinkMethodBody = SubmitSelfServiceRecoveryFlowWithLinkMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Email to Recover  Needs to be set when initiating the flow. If the email is a registered recovery email, a recovery link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
    email :: Text,
    -- | Method supports `link` only right now.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRecoveryFlowWithLinkMethodBody

instance ToJSON SubmitSelfServiceRecoveryFlowWithLinkMethodBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceRegistrationFlowBody = SubmitSelfServiceRegistrationFlowBody
  { -- | CSRFToken is the anti-CSRF token
    csrf_token :: Maybe Text,
    -- | Method  Should be set to \"webauthn\" when trying to add, update, or remove a webAuthn pairing.
    method :: Text,
    -- | Password to sign the user up with
    password :: Text,
    -- | The identity's traits
    traits :: Value,
    -- | The provider to register with
    provider :: Text,
    -- | Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
    webauthn_register :: Maybe Text,
    -- | Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
    webauthn_register_displayname :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowBody

instance ToJSON SubmitSelfServiceRegistrationFlowBody where
  toEncoding = genericToEncoding defaultOptions

-- | SubmitSelfServiceRegistrationFlowWithOidcMethodBody is used to decode the registration form payload when using the oidc method.
data SubmitSelfServiceRegistrationFlowWithOidcMethodBody = SubmitSelfServiceRegistrationFlowWithOidcMethodBody
  { -- | The CSRF Token
    csrf_token :: Maybe Text,
    -- | Method to use  This field must be set to `oidc` when using the oidc method.
    method :: Text,
    -- | The provider to register with
    provider :: Text,
    -- | The identity traits
    traits :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithOidcMethodBody

instance ToJSON SubmitSelfServiceRegistrationFlowWithOidcMethodBody where
  toEncoding = genericToEncoding defaultOptions

-- | SubmitSelfServiceRegistrationFlowWithPasswordMethodBody is used to decode the registration form payload when using the password method.
data SubmitSelfServiceRegistrationFlowWithPasswordMethodBody = SubmitSelfServiceRegistrationFlowWithPasswordMethodBody
  { -- | The CSRF Token
    csrf_token :: Maybe Text,
    -- | Method to use  This field must be set to `password` when using the password method.
    method :: Text,
    -- | Password to sign the user up with
    password :: Text,
    -- | The identity's traits
    traits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithPasswordMethodBody

instance ToJSON SubmitSelfServiceRegistrationFlowWithPasswordMethodBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody = SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody
  { -- | CSRFToken is the anti-CSRF token
    csrf_token :: Maybe Text,
    -- | Method  Should be set to \"webauthn\" when trying to add, update, or remove a webAuthn pairing.
    method :: Text,
    -- | The identity's traits
    traits :: Value,
    -- | Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
    webauthn_register :: Maybe Text,
    -- | Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
    webauthn_register_displayname :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody

instance ToJSON SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceSettingsFlowBody = SubmitSelfServiceSettingsFlowBody
  { -- | CSRFToken is the anti-CSRF token
    csrf_token :: Maybe Text,
    -- | Method  Should be set to \"lookup\" when trying to add, update, or remove a lookup pairing.
    method :: Text,
    -- | Password is the updated password
    password :: Text,
    -- | The identity's traits  in: body
    traits :: Value,
    -- | Flow ID is the flow's ID.  in: query
    flow :: Maybe Text,
    -- | Link this provider  Either this or `unlink` must be set.  type: string in: body
    link :: Maybe Text,
    -- | Unlink this provider  Either this or `link` must be set.  type: string in: body
    unlink :: Maybe Text,
    -- | ValidationTOTP must contain a valid TOTP based on the
    totp_code :: Maybe Text,
    -- | UnlinkTOTP if true will remove the TOTP pairing, effectively removing the credential. This can be used to set up a new TOTP device.
    totp_unlink :: Maybe Bool,
    -- | Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
    webauthn_register :: Maybe Text,
    -- | Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
    webauthn_register_displayname :: Maybe Text,
    -- | Remove a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
    webauthn_remove :: Maybe Text,
    -- | If set to true will save the regenerated lookup secrets
    lookup_secret_confirm :: Maybe Bool,
    -- | Disables this method if true.
    lookup_secret_disable :: Maybe Bool,
    -- | If set to true will regenerate the lookup secrets
    lookup_secret_regenerate :: Maybe Bool,
    -- | If set to true will reveal the lookup secrets
    lookup_secret_reveal :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowBody

instance ToJSON SubmitSelfServiceSettingsFlowBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceSettingsFlowWithLookupMethodBody = SubmitSelfServiceSettingsFlowWithLookupMethodBody
  { -- | CSRFToken is the anti-CSRF token
    csrf_token :: Maybe Text,
    -- | If set to true will save the regenerated lookup secrets
    lookup_secret_confirm :: Maybe Bool,
    -- | Disables this method if true.
    lookup_secret_disable :: Maybe Bool,
    -- | If set to true will regenerate the lookup secrets
    lookup_secret_regenerate :: Maybe Bool,
    -- | If set to true will reveal the lookup secrets
    lookup_secret_reveal :: Maybe Bool,
    -- | Method  Should be set to \"lookup\" when trying to add, update, or remove a lookup pairing.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithLookupMethodBody

instance ToJSON SubmitSelfServiceSettingsFlowWithLookupMethodBody where
  toEncoding = genericToEncoding defaultOptions

-- | nolint:deadcode,unused
data SubmitSelfServiceSettingsFlowWithOidcMethodBody = SubmitSelfServiceSettingsFlowWithOidcMethodBody
  { -- | Flow ID is the flow's ID.  in: query
    flow :: Maybe Text,
    -- | Link this provider  Either this or `unlink` must be set.  type: string in: body
    link :: Maybe Text,
    -- | Method  Should be set to profile when trying to update a profile.
    method :: Text,
    -- | The identity's traits  in: body
    traits :: Maybe Value,
    -- | Unlink this provider  Either this or `link` must be set.  type: string in: body
    unlink :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithOidcMethodBody

instance ToJSON SubmitSelfServiceSettingsFlowWithOidcMethodBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceSettingsFlowWithPasswordMethodBody = SubmitSelfServiceSettingsFlowWithPasswordMethodBody
  { -- | CSRFToken is the anti-CSRF token
    csrf_token :: Maybe Text,
    -- | Method  Should be set to password when trying to update a password.
    method :: Text,
    -- | Password is the updated password
    password :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithPasswordMethodBody

instance ToJSON SubmitSelfServiceSettingsFlowWithPasswordMethodBody where
  toEncoding = genericToEncoding defaultOptions

-- | nolint:deadcode,unused
data SubmitSelfServiceSettingsFlowWithProfileMethodBody = SubmitSelfServiceSettingsFlowWithProfileMethodBody
  { -- | The Anti-CSRF Token  This token is only required when performing browser flows.
    csrf_token :: Maybe Text,
    -- | Method  Should be set to profile when trying to update a profile.
    method :: Text,
    -- | Traits contains all of the identity's traits.
    traits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithProfileMethodBody

instance ToJSON SubmitSelfServiceSettingsFlowWithProfileMethodBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceSettingsFlowWithTotpMethodBody = SubmitSelfServiceSettingsFlowWithTotpMethodBody
  { -- | CSRFToken is the anti-CSRF token
    csrf_token :: Maybe Text,
    -- | Method  Should be set to \"totp\" when trying to add, update, or remove a totp pairing.
    method :: Text,
    -- | ValidationTOTP must contain a valid TOTP based on the
    totp_code :: Maybe Text,
    -- | UnlinkTOTP if true will remove the TOTP pairing, effectively removing the credential. This can be used to set up a new TOTP device.
    totp_unlink :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithTotpMethodBody

instance ToJSON SubmitSelfServiceSettingsFlowWithTotpMethodBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody = SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody
  { -- | CSRFToken is the anti-CSRF token
    csrf_token :: Maybe Text,
    -- | Method  Should be set to \"webauthn\" when trying to add, update, or remove a webAuthn pairing.
    method :: Text,
    -- | Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
    webauthn_register :: Maybe Text,
    -- | Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
    webauthn_register_displayname :: Maybe Text,
    -- | Remove a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
    webauthn_remove :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody

instance ToJSON SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody where
  toEncoding = genericToEncoding defaultOptions

-- | nolint:deadcode,unused
data SubmitSelfServiceVerificationFlowBody = SubmitSelfServiceVerificationFlowBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Email to Verify  Needs to be set when initiating the flow. If the email is a registered verification email, a verification link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
    email :: Text,
    -- | Method supports `link` only right now.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceVerificationFlowBody

instance ToJSON SubmitSelfServiceVerificationFlowBody where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceVerificationFlowWithLinkMethodBody = SubmitSelfServiceVerificationFlowWithLinkMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Email to Verify  Needs to be set when initiating the flow. If the email is a registered verification email, a verification link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
    email :: Text,
    -- | Method supports `link` only right now.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceVerificationFlowWithLinkMethodBody

instance ToJSON SubmitSelfServiceVerificationFlowWithLinkMethodBody where
  toEncoding = genericToEncoding defaultOptions

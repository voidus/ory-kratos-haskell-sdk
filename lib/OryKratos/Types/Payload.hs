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
    submitSelfServiceLoginFlowBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Identifier is the email or username of the user trying to log in. This field is only required when using WebAuthn for passwordless login. When using WebAuthn for multi-factor authentication, it is not needed.
    submitSelfServiceLoginFlowBodyIdentifier :: Text,
    -- | Method should be set to \"lookup_secret\" when logging in using the lookup_secret strategy.
    submitSelfServiceLoginFlowBodyMethod :: Text,
    -- | The user's password.
    submitSelfServiceLoginFlowBodyPassword :: Text,
    -- | Identifier is the email or username of the user trying to log in. This field is deprecated!
    submitSelfServiceLoginFlowBodyPasswordUnderscoreidentifier :: Maybe Text,
    -- | The provider to register with
    submitSelfServiceLoginFlowBodyProvider :: Text,
    -- | The identity traits. This is a placeholder for the registration flow.
    submitSelfServiceLoginFlowBodyTraits :: Maybe Value,
    -- | The TOTP code.
    submitSelfServiceLoginFlowBodyTotpUnderscorecode :: Text,
    -- | Login a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
    submitSelfServiceLoginFlowBodyWebauthnUnderscorelogin :: Maybe Text,
    -- | The lookup secret.
    submitSelfServiceLoginFlowBodyLookupUnderscoresecret :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowBody")

instance ToJSON SubmitSelfServiceLoginFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceLoginFlowBody")

data SubmitSelfServiceLoginFlowWithLookupSecretMethodBody = SubmitSelfServiceLoginFlowWithLookupSecretMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    submitSelfServiceLoginFlowWithLookupSecretMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | The lookup secret.
    submitSelfServiceLoginFlowWithLookupSecretMethodBodyLookupUnderscoresecret :: Text,
    -- | Method should be set to \"lookup_secret\" when logging in using the lookup_secret strategy.
    submitSelfServiceLoginFlowWithLookupSecretMethodBodyMethod :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithLookupSecretMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithLookupSecretMethodBody")

instance ToJSON SubmitSelfServiceLoginFlowWithLookupSecretMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithLookupSecretMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithLookupSecretMethodBody")

-- | SubmitSelfServiceLoginFlowWithOidcMethodBody is used to decode the login form payload when using the oidc method.
data SubmitSelfServiceLoginFlowWithOidcMethodBody = SubmitSelfServiceLoginFlowWithOidcMethodBody
  { -- | The CSRF Token
    submitSelfServiceLoginFlowWithOidcMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method to use  This field must be set to `oidc` when using the oidc method.
    submitSelfServiceLoginFlowWithOidcMethodBodyMethod :: Text,
    -- | The provider to register with
    submitSelfServiceLoginFlowWithOidcMethodBodyProvider :: Text,
    -- | The identity traits. This is a placeholder for the registration flow.
    submitSelfServiceLoginFlowWithOidcMethodBodyTraits :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithOidcMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithOidcMethodBody")

instance ToJSON SubmitSelfServiceLoginFlowWithOidcMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithOidcMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithOidcMethodBody")

data SubmitSelfServiceLoginFlowWithPasswordMethodBody = SubmitSelfServiceLoginFlowWithPasswordMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    submitSelfServiceLoginFlowWithPasswordMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Identifier is the email or username of the user trying to log in.
    submitSelfServiceLoginFlowWithPasswordMethodBodyIdentifier :: Text,
    -- | Method should be set to \"password\" when logging in using the identifier and password strategy.
    submitSelfServiceLoginFlowWithPasswordMethodBodyMethod :: Text,
    -- | The user's password.
    submitSelfServiceLoginFlowWithPasswordMethodBodyPassword :: Text,
    -- | Identifier is the email or username of the user trying to log in. This field is deprecated!
    submitSelfServiceLoginFlowWithPasswordMethodBodyPasswordUnderscoreidentifier :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithPasswordMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithPasswordMethodBody")

instance ToJSON SubmitSelfServiceLoginFlowWithPasswordMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithPasswordMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithPasswordMethodBody")

data SubmitSelfServiceLoginFlowWithTotpMethodBody = SubmitSelfServiceLoginFlowWithTotpMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    submitSelfServiceLoginFlowWithTotpMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method should be set to \"totp\" when logging in using the TOTP strategy.
    submitSelfServiceLoginFlowWithTotpMethodBodyMethod :: Text,
    -- | The TOTP code.
    submitSelfServiceLoginFlowWithTotpMethodBodyTotpUnderscorecode :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithTotpMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithTotpMethodBody")

instance ToJSON SubmitSelfServiceLoginFlowWithTotpMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithTotpMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithTotpMethodBody")

data SubmitSelfServiceLoginFlowWithWebAuthnMethodBody = SubmitSelfServiceLoginFlowWithWebAuthnMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    submitSelfServiceLoginFlowWithWebAuthnMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Identifier is the email or username of the user trying to log in. This field is only required when using WebAuthn for passwordless login. When using WebAuthn for multi-factor authentication, it is not needed.
    submitSelfServiceLoginFlowWithWebAuthnMethodBodyIdentifier :: Maybe Text,
    -- | Method should be set to \"webAuthn\" when logging in using the WebAuthn strategy.
    submitSelfServiceLoginFlowWithWebAuthnMethodBodyMethod :: Text,
    -- | Login a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
    submitSelfServiceLoginFlowWithWebAuthnMethodBodyWebauthnUnderscorelogin :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithWebAuthnMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLoginFlowWithWebAuthnMethodBody")

instance ToJSON SubmitSelfServiceLoginFlowWithWebAuthnMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithWebAuthnMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceLoginFlowWithWebAuthnMethodBody")

-- | nolint:deadcode,unused
data SubmitSelfServiceLogoutFlowWithoutBrowserBody = SubmitSelfServiceLogoutFlowWithoutBrowserBody
  { -- | The Session Token  Invalidate this session token.
    submitSelfServiceLogoutFlowWithoutBrowserBodySessionUnderscoretoken :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLogoutFlowWithoutBrowserBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceLogoutFlowWithoutBrowserBody")

instance ToJSON SubmitSelfServiceLogoutFlowWithoutBrowserBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceLogoutFlowWithoutBrowserBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceLogoutFlowWithoutBrowserBody")

data SubmitSelfServiceRecoveryFlowBody = SubmitSelfServiceRecoveryFlowBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    submitSelfServiceRecoveryFlowBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Email to Recover  Needs to be set when initiating the flow. If the email is a registered recovery email, a recovery link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
    submitSelfServiceRecoveryFlowBodyEmail :: Text,
    -- | Method supports `link` only right now.
    submitSelfServiceRecoveryFlowBodyMethod :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRecoveryFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRecoveryFlowBody")

instance ToJSON SubmitSelfServiceRecoveryFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRecoveryFlowBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceRecoveryFlowBody")

data SubmitSelfServiceRecoveryFlowWithLinkMethodBody = SubmitSelfServiceRecoveryFlowWithLinkMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    submitSelfServiceRecoveryFlowWithLinkMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Email to Recover  Needs to be set when initiating the flow. If the email is a registered recovery email, a recovery link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
    submitSelfServiceRecoveryFlowWithLinkMethodBodyEmail :: Text,
    -- | Method supports `link` only right now.
    submitSelfServiceRecoveryFlowWithLinkMethodBodyMethod :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRecoveryFlowWithLinkMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRecoveryFlowWithLinkMethodBody")

instance ToJSON SubmitSelfServiceRecoveryFlowWithLinkMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRecoveryFlowWithLinkMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowBody")

data SubmitSelfServiceRegistrationFlowBody = SubmitSelfServiceRegistrationFlowBody
  { -- | CSRFToken is the anti-CSRF token
    submitSelfServiceRegistrationFlowBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method  Should be set to \"webauthn\" when trying to add, update, or remove a webAuthn pairing.
    submitSelfServiceRegistrationFlowBodyMethod :: Text,
    -- | Password to sign the user up with
    submitSelfServiceRegistrationFlowBodyPassword :: Text,
    -- | The identity's traits
    submitSelfServiceRegistrationFlowBodyTraits :: Value,
    -- | The provider to register with
    submitSelfServiceRegistrationFlowBodyProvider :: Text,
    -- | Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
    submitSelfServiceRegistrationFlowBodyWebauthnUnderscoreregister :: Maybe Text,
    -- | Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
    submitSelfServiceRegistrationFlowBodyWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRegistrationFlowBody")

instance ToJSON SubmitSelfServiceRegistrationFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowBody")

-- | SubmitSelfServiceRegistrationFlowWithOidcMethodBody is used to decode the registration form payload when using the oidc method.
data SubmitSelfServiceRegistrationFlowWithOidcMethodBody = SubmitSelfServiceRegistrationFlowWithOidcMethodBody
  { -- | The CSRF Token
    submitSelfServiceRegistrationFlowWithOidcMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method to use  This field must be set to `oidc` when using the oidc method.
    submitSelfServiceRegistrationFlowWithOidcMethodBodyMethod :: Text,
    -- | The provider to register with
    submitSelfServiceRegistrationFlowWithOidcMethodBodyProvider :: Text,
    -- | The identity traits
    submitSelfServiceRegistrationFlowWithOidcMethodBodyTraits :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithOidcMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRegistrationFlowWithOidcMethodBody")

instance ToJSON SubmitSelfServiceRegistrationFlowWithOidcMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowWithOidcMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowWithOidcMethodBody")

-- | SubmitSelfServiceRegistrationFlowWithPasswordMethodBody is used to decode the registration form payload when using the password method.
data SubmitSelfServiceRegistrationFlowWithPasswordMethodBody = SubmitSelfServiceRegistrationFlowWithPasswordMethodBody
  { -- | The CSRF Token
    submitSelfServiceRegistrationFlowWithPasswordMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method to use  This field must be set to `password` when using the password method.
    submitSelfServiceRegistrationFlowWithPasswordMethodBodyMethod :: Text,
    -- | Password to sign the user up with
    submitSelfServiceRegistrationFlowWithPasswordMethodBodyPassword :: Text,
    -- | The identity's traits
    submitSelfServiceRegistrationFlowWithPasswordMethodBodyTraits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithPasswordMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRegistrationFlowWithPasswordMethodBody")

instance ToJSON SubmitSelfServiceRegistrationFlowWithPasswordMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowWithPasswordMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowWithPasswordMethodBody")

data SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody = SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody
  { -- | CSRFToken is the anti-CSRF token
    submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method  Should be set to \"webauthn\" when trying to add, update, or remove a webAuthn pairing.
    submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyMethod :: Text,
    -- | The identity's traits
    submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyTraits :: Value,
    -- | Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
    submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyWebauthnUnderscoreregister :: Maybe Text,
    -- | Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
    submitSelfServiceRegistrationFlowWithWebAuthnMethodBodyWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceRegistrationFlowWithWebAuthnMethodBody")

instance ToJSON SubmitSelfServiceRegistrationFlowWithWebAuthnMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowWithWebAuthnMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceRegistrationFlowWithWebAuthnMethodBody")

data SubmitSelfServiceSettingsFlowBody = SubmitSelfServiceSettingsFlowBody
  { -- | CSRFToken is the anti-CSRF token
    submitSelfServiceSettingsFlowBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method  Should be set to \"lookup\" when trying to add, update, or remove a lookup pairing.
    submitSelfServiceSettingsFlowBodyMethod :: Text,
    -- | Password is the updated password
    submitSelfServiceSettingsFlowBodyPassword :: Text,
    -- | The identity's traits  in: body
    submitSelfServiceSettingsFlowBodyTraits :: Value,
    -- | Flow ID is the flow's ID.  in: query
    submitSelfServiceSettingsFlowBodyFlow :: Maybe Text,
    -- | Link this provider  Either this or `unlink` must be set.  type: string in: body
    submitSelfServiceSettingsFlowBodyLink :: Maybe Text,
    -- | Unlink this provider  Either this or `link` must be set.  type: string in: body
    submitSelfServiceSettingsFlowBodyUnlink :: Maybe Text,
    -- | ValidationTOTP must contain a valid TOTP based on the
    submitSelfServiceSettingsFlowBodyTotpUnderscorecode :: Maybe Text,
    -- | UnlinkTOTP if true will remove the TOTP pairing, effectively removing the credential. This can be used to set up a new TOTP device.
    submitSelfServiceSettingsFlowBodyTotpUnderscoreunlink :: Maybe Bool,
    -- | Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
    submitSelfServiceSettingsFlowBodyWebauthnUnderscoreregister :: Maybe Text,
    -- | Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
    submitSelfServiceSettingsFlowBodyWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text,
    -- | Remove a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
    submitSelfServiceSettingsFlowBodyWebauthnUnderscoreremove :: Maybe Text,
    -- | If set to true will save the regenerated lookup secrets
    submitSelfServiceSettingsFlowBodyLookupUnderscoresecretUnderscoreconfirm :: Maybe Bool,
    -- | Disables this method if true.
    submitSelfServiceSettingsFlowBodyLookupUnderscoresecretUnderscoredisable :: Maybe Bool,
    -- | If set to true will regenerate the lookup secrets
    submitSelfServiceSettingsFlowBodyLookupUnderscoresecretUnderscoreregenerate :: Maybe Bool,
    -- | If set to true will reveal the lookup secrets
    submitSelfServiceSettingsFlowBodyLookupUnderscoresecretUnderscorereveal :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowBody")

instance ToJSON SubmitSelfServiceSettingsFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowBody")

data SubmitSelfServiceSettingsFlowWithLookupMethodBody = SubmitSelfServiceSettingsFlowWithLookupMethodBody
  { -- | CSRFToken is the anti-CSRF token
    submitSelfServiceSettingsFlowWithLookupMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | If set to true will save the regenerated lookup secrets
    submitSelfServiceSettingsFlowWithLookupMethodBodyLookupUnderscoresecretUnderscoreconfirm :: Maybe Bool,
    -- | Disables this method if true.
    submitSelfServiceSettingsFlowWithLookupMethodBodyLookupUnderscoresecretUnderscoredisable :: Maybe Bool,
    -- | If set to true will regenerate the lookup secrets
    submitSelfServiceSettingsFlowWithLookupMethodBodyLookupUnderscoresecretUnderscoreregenerate :: Maybe Bool,
    -- | If set to true will reveal the lookup secrets
    submitSelfServiceSettingsFlowWithLookupMethodBodyLookupUnderscoresecretUnderscorereveal :: Maybe Bool,
    -- | Method  Should be set to \"lookup\" when trying to add, update, or remove a lookup pairing.
    submitSelfServiceSettingsFlowWithLookupMethodBodyMethod :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithLookupMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithLookupMethodBody")

instance ToJSON SubmitSelfServiceSettingsFlowWithLookupMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithLookupMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithLookupMethodBody")

-- | nolint:deadcode,unused
data SubmitSelfServiceSettingsFlowWithOidcMethodBody = SubmitSelfServiceSettingsFlowWithOidcMethodBody
  { -- | Flow ID is the flow's ID.  in: query
    submitSelfServiceSettingsFlowWithOidcMethodBodyFlow :: Maybe Text,
    -- | Link this provider  Either this or `unlink` must be set.  type: string in: body
    submitSelfServiceSettingsFlowWithOidcMethodBodyLink :: Maybe Text,
    -- | Method  Should be set to profile when trying to update a profile.
    submitSelfServiceSettingsFlowWithOidcMethodBodyMethod :: Text,
    -- | The identity's traits  in: body
    submitSelfServiceSettingsFlowWithOidcMethodBodyTraits :: Maybe Value,
    -- | Unlink this provider  Either this or `link` must be set.  type: string in: body
    submitSelfServiceSettingsFlowWithOidcMethodBodyUnlink :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithOidcMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithOidcMethodBody")

instance ToJSON SubmitSelfServiceSettingsFlowWithOidcMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithOidcMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithOidcMethodBody")

data SubmitSelfServiceSettingsFlowWithPasswordMethodBody = SubmitSelfServiceSettingsFlowWithPasswordMethodBody
  { -- | CSRFToken is the anti-CSRF token
    submitSelfServiceSettingsFlowWithPasswordMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method  Should be set to password when trying to update a password.
    submitSelfServiceSettingsFlowWithPasswordMethodBodyMethod :: Text,
    -- | Password is the updated password
    submitSelfServiceSettingsFlowWithPasswordMethodBodyPassword :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithPasswordMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithPasswordMethodBody")

instance ToJSON SubmitSelfServiceSettingsFlowWithPasswordMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithPasswordMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithPasswordMethodBody")

-- | nolint:deadcode,unused
data SubmitSelfServiceSettingsFlowWithProfileMethodBody = SubmitSelfServiceSettingsFlowWithProfileMethodBody
  { -- | The Anti-CSRF Token  This token is only required when performing browser flows.
    submitSelfServiceSettingsFlowWithProfileMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method  Should be set to profile when trying to update a profile.
    submitSelfServiceSettingsFlowWithProfileMethodBodyMethod :: Text,
    -- | Traits contains all of the identity's traits.
    submitSelfServiceSettingsFlowWithProfileMethodBodyTraits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithProfileMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithProfileMethodBody")

instance ToJSON SubmitSelfServiceSettingsFlowWithProfileMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithProfileMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithProfileMethodBody")

data SubmitSelfServiceSettingsFlowWithTotpMethodBody = SubmitSelfServiceSettingsFlowWithTotpMethodBody
  { -- | CSRFToken is the anti-CSRF token
    submitSelfServiceSettingsFlowWithTotpMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method  Should be set to \"totp\" when trying to add, update, or remove a totp pairing.
    submitSelfServiceSettingsFlowWithTotpMethodBodyMethod :: Text,
    -- | ValidationTOTP must contain a valid TOTP based on the
    submitSelfServiceSettingsFlowWithTotpMethodBodyTotpUnderscorecode :: Maybe Text,
    -- | UnlinkTOTP if true will remove the TOTP pairing, effectively removing the credential. This can be used to set up a new TOTP device.
    submitSelfServiceSettingsFlowWithTotpMethodBodyTotpUnderscoreunlink :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithTotpMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithTotpMethodBody")

instance ToJSON SubmitSelfServiceSettingsFlowWithTotpMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithTotpMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithTotpMethodBody")

data SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody = SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody
  { -- | CSRFToken is the anti-CSRF token
    submitSelfServiceSettingsFlowWithWebAuthnMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Method  Should be set to \"webauthn\" when trying to add, update, or remove a webAuthn pairing.
    submitSelfServiceSettingsFlowWithWebAuthnMethodBodyMethod :: Text,
    -- | Register a WebAuthn Security Key  It is expected that the JSON returned by the WebAuthn registration process is included here.
    submitSelfServiceSettingsFlowWithWebAuthnMethodBodyWebauthnUnderscoreregister :: Maybe Text,
    -- | Name of the WebAuthn Security Key to be Added  A human-readable name for the security key which will be added.
    submitSelfServiceSettingsFlowWithWebAuthnMethodBodyWebauthnUnderscoreregisterUnderscoredisplayname :: Maybe Text,
    -- | Remove a WebAuthn Security Key  This must contain the ID of the WebAuthN connection.
    submitSelfServiceSettingsFlowWithWebAuthnMethodBodyWebauthnUnderscoreremove :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceSettingsFlowWithWebAuthnMethodBody")

instance ToJSON SubmitSelfServiceSettingsFlowWithWebAuthnMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithWebAuthnMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceSettingsFlowWithWebAuthnMethodBody")

-- | nolint:deadcode,unused
data SubmitSelfServiceVerificationFlowBody = SubmitSelfServiceVerificationFlowBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    submitSelfServiceVerificationFlowBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Email to Verify  Needs to be set when initiating the flow. If the email is a registered verification email, a verification link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
    submitSelfServiceVerificationFlowBodyEmail :: Text,
    -- | Method supports `link` only right now.
    submitSelfServiceVerificationFlowBodyMethod :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceVerificationFlowBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceVerificationFlowBody")

instance ToJSON SubmitSelfServiceVerificationFlowBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceVerificationFlowBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceVerificationFlowBody")

data SubmitSelfServiceVerificationFlowWithLinkMethodBody = SubmitSelfServiceVerificationFlowWithLinkMethodBody
  { -- | Sending the anti-csrf token is only required for browser login flows.
    submitSelfServiceVerificationFlowWithLinkMethodBodyCsrfUnderscoretoken :: Maybe Text,
    -- | Email to Verify  Needs to be set when initiating the flow. If the email is a registered verification email, a verification link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email
    submitSelfServiceVerificationFlowWithLinkMethodBodyEmail :: Text,
    -- | Method supports `link` only right now.
    submitSelfServiceVerificationFlowWithLinkMethodBodyMethod :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceVerificationFlowWithLinkMethodBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submitSelfServiceVerificationFlowWithLinkMethodBody")

instance ToJSON SubmitSelfServiceVerificationFlowWithLinkMethodBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submitSelfServiceVerificationFlowWithLinkMethodBody")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "submitSelfServiceVerificationFlowWithLinkMethodBody")

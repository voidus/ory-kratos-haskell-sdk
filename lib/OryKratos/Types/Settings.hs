module OryKratos.Types.Settings
  ( SettingsFlow (..),
    SettingsViaApiResponse (..),
    SubmitSelfServiceBrowserSettingsOIDCFlowPayload (..),
    SubmitSelfServiceSettingsFlowWithPasswordMethod (..),
  )
where

import OryKratos.Types.Misc (Identity)
import OryKratos.Types.Ui (UiContainer)
import Pre

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
    -- | RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- |
    state :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text,
    -- |
    ui :: UiContainer
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
data SubmitSelfServiceBrowserSettingsOIDCFlowPayload = SubmitSelfServiceBrowserSettingsOIDCFlowPayload
  { -- | Flow ID is the flow's ID.  in: query
    flow :: Maybe Text,
    -- | Link this provider  Either this or `unlink` must be set.  type: string in: body
    link :: Maybe Text,
    -- | Unlink this provider  Either this or `link` must be set.  type: string in: body
    unlink :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceBrowserSettingsOIDCFlowPayload

instance ToJSON SubmitSelfServiceBrowserSettingsOIDCFlowPayload where
  toEncoding = genericToEncoding defaultOptions

data SubmitSelfServiceSettingsFlowWithPasswordMethod = SubmitSelfServiceSettingsFlowWithPasswordMethod
  { -- | CSRFToken is the anti-CSRF token  type: string
    csrf_token :: Maybe Text,
    -- | Method  Should be set to password when trying to update a password.  type: string
    method :: Maybe Text,
    -- | Password is the updated password  type: string
    password :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceSettingsFlowWithPasswordMethod

instance ToJSON SubmitSelfServiceSettingsFlowWithPasswordMethod where
  toEncoding = genericToEncoding defaultOptions
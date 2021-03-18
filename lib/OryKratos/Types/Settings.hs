module OryKratos.Types.Settings where

import Pre
import OryKratos.Types.Misc ( FormField, Message, Identity )


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

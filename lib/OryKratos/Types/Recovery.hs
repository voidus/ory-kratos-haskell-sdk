module OryKratos.Types.Recovery
  ( RecoveryFlow (..),
    RecoveryFlowMethods (..),
    RecoveryFlowMethod (..),
    RecoveryFlowMethodConfig (..),
    RecoveryLink (..),
  )
where

import OryKratos.Types.Misc (FormField, Message)
import Pre

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
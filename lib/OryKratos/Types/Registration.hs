module OryKratos.Types.Registration
  ( RegistrationFlow (..),
    RegistrationFlowMethods (..),
    RegistrationFlowMethod (..),
    RegistrationFlowMethodConfig (..),
    RegistrationViaApiResponse (..),
  )
where

import OryKratos.Types.Misc
  ( FormField,
    Identity,
    Message,
    Session,
  )
import Pre

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

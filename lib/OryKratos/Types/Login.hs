module OryKratos.Types.Login where

import Pre
import OryKratos.Types.Misc ( FormField, Message, Session )

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
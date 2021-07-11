module OryKratos.Types.Login
  ( LoginFlow (..),
    LoginViaApiResponse (..),
    AuthenticateOKBody (..),
    SubmitSelfServiceLoginFlowWithPasswordMethod (..),
  )
where

import OryKratos.Types.Misc (Session)
import OryKratos.Types.Ui (UiContainer)
import Pre

-- | This object represents a login flow. A login flow is initiated at the \&quot;Initiate Login API / Browser Flow\&quot; endpoint by a client.  Once a login flow is completed successfully, a session cookie or session token will be issued.
data LoginFlow = LoginFlow
  { -- | and so on.
    active :: Maybe Text,
    -- | CreatedAt is a helper struct field for gobuffalo.pop.
    created_at :: Maybe UTCTime,
    -- | ExpiresAt is the time (UTC) when the flow expires. If the user still wishes to log in, a new flow has to be initiated.
    expires_at :: UTCTime,
    -- | Forced stores whether this login flow should enforce re-authentication.
    forced :: Maybe Bool,
    -- |
    id :: Text,
    -- | IssuedAt is the time (UTC) when the flow started.
    issued_at :: UTCTime,
    -- | RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Text,
    -- |
    ui :: UiContainer,
    -- | UpdatedAt is a helper struct field for gobuffalo.pop.
    updated_at :: Maybe UTCTime
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

-- | AuthenticateOKBody authenticate o k body
data AuthenticateOKBody = AuthenticateOKBody
  { -- | An opaque token used to authenticate a user after a successful login
    identity_token :: Text,
    -- | The status of the authentication
    status :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AuthenticateOKBody

instance ToJSON AuthenticateOKBody where
  toEncoding = genericToEncoding defaultOptions

-- |
data SubmitSelfServiceLoginFlowWithPasswordMethod = SubmitSelfServiceLoginFlowWithPasswordMethod
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Method should be set to \"password\" when logging in using the identifier and password strategy.
    method :: Maybe Text,
    -- | The user's password.
    password :: Maybe Text,
    -- | Identifier is the email or username of the user trying to log in.
    password_identifier :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceLoginFlowWithPasswordMethod

instance ToJSON SubmitSelfServiceLoginFlowWithPasswordMethod where
  toEncoding = genericToEncoding defaultOptions

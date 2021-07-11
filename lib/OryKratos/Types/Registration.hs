module OryKratos.Types.Registration
  ( RegistrationFlow (..),
    RegistrationViaApiResponse (..),
    SubmitSelfServiceRegistrationFlowWithPasswordMethod (..),
  )
where

import OryKratos.Types.Misc
  ( Identity,
    Session,
  )
import OryKratos.Types.Ui (UiContainer)
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
    -- | RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text,
    -- |
    ui :: UiContainer
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

-- | SubmitSelfServiceRegistrationFlowWithPasswordMethod is used to decode the registration form payload when using the password method.
data SubmitSelfServiceRegistrationFlowWithPasswordMethod = SubmitSelfServiceRegistrationFlowWithPasswordMethod
  { -- | The CSRF Token
    csrf_token :: Maybe Text,
    -- | Method to use  This field must be set to `password` when using the password method.
    method :: Text,
    -- | Password to sign the user up with
    password :: Maybe Text,
    -- | The identity's traits
    traits :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRegistrationFlowWithPasswordMethod

instance ToJSON SubmitSelfServiceRegistrationFlowWithPasswordMethod where
  toEncoding = genericToEncoding defaultOptions
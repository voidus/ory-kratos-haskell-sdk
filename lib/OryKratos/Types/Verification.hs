module OryKratos.Types.Verification
  ( VerificationFlow (..),
    VerificationFlowMethods (..),
    VerificationFlowMethod (..),
    VerificationFlowMethodConfig (..),
  )
where

import OryKratos.Types.Misc (FormField, Message)
import Pre

-- | Used to verify an out-of-band communication channel such as an email address or a phone number.  For more information head over to: https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation
data VerificationFlow = VerificationFlow
  { -- | Active, if set, contains the registration method that is being used. It is initially not set.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the request expires. If the user still wishes to verify the address, a new request has to be initiated.
    expires_at :: Maybe UTCTime,
    -- |
    id :: Maybe UUID,
    -- | IssuedAt is the time (UTC) when the request occurred.
    issued_at :: Maybe UTCTime,
    -- |
    messages :: Maybe [Message],
    -- | Methods contains context for all account verification methods. If a registration request has been processed, but for example the password is incorrect, this will contain error messages.
    methods :: Map String VerificationFlowMethod,
    -- | RequestURL is the initial URL that was requested from ORY Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Maybe Text,
    -- |
    state :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerificationFlow where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON VerificationFlow where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- |
data VerificationFlowMethods = VerificationFlowMethods
  { -- |
    link :: Maybe VerificationFlowMethod
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerificationFlowMethods

instance ToJSON VerificationFlowMethods where
  toEncoding = genericToEncoding defaultOptions

-- |
data VerificationFlowMethod = VerificationFlowMethod
  { -- |
    config :: VerificationFlowMethodConfig,
    -- | Method contains the request credentials type.
    method :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerificationFlowMethod

instance ToJSON VerificationFlowMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data VerificationFlowMethodConfig = VerificationFlowMethodConfig
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

instance FromJSON VerificationFlowMethodConfig

instance ToJSON VerificationFlowMethodConfig where
  toEncoding = genericToEncoding defaultOptions
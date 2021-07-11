module OryKratos.Types.Verification
  ( VerificationFlow (..),
  )
where

import OryKratos.Types.Ui (UiContainer)
import Pre

-- | Used to verify an out-of-band communication channel such as an email address or a phone number.  For more information head over to: https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation
data VerificationFlow = VerificationFlow
  { -- | Active, if set, contains the registration method that is being used. It is initially not set.
    active :: Maybe Text,
    -- | ExpiresAt is the time (UTC) when the request expires. If the user still wishes to verify the address, a new request has to be initiated.
    expires_at :: Maybe UTCTime,
    -- |
    id :: Text,
    -- | IssuedAt is the time (UTC) when the request occurred.
    issued_at :: Maybe UTCTime,
    -- | RequestURL is the initial URL that was requested from Ory Kratos. It can be used to forward information contained in the URL's path or query for example.
    request_url :: Maybe Text,
    -- |
    state :: Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Text,
    -- |
    ui :: UiContainer
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

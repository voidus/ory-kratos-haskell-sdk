module OryKratos.Types.Recovery
  ( RecoveryFlow (..),
    RecoveryLink (..),
    SubmitSelfServiceRecoveryFlowWithLinkMethod (..),
  )
where

import OryKratos.Types.Ui (UiContainer)
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

-- |
data SubmitSelfServiceRecoveryFlowWithLinkMethod = SubmitSelfServiceRecoveryFlowWithLinkMethod
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Email to Recover  Needs to be set when initiating the flow. If the email is a registered recovery email, a recovery link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email in: body
    email :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SubmitSelfServiceRecoveryFlowWithLinkMethod

instance ToJSON SubmitSelfServiceRecoveryFlowWithLinkMethod where
  toEncoding = genericToEncoding defaultOptions
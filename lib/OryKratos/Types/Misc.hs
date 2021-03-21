module OryKratos.Types.Misc
  ( Message (..),
    ErrorContainer (..),
    FormField (..),
    GenericError (..),
    GenericErrorPayload (..),
    HealthNotReadyStatus (..),
    HealthStatus (..),
    CompleteSelfServiceLoginFlowWithPasswordMethod (..),
    CompleteSelfServiceRecoveryFlowWithLinkMethod (..),
    CompleteSelfServiceSettingsFlowWithPasswordMethod (..),
    CompleteSelfServiceVerificationFlowWithLinkMethod (..),
    CreateIdentity (..),
    CreateRecoveryLink (..),
    Identity (..),
    RevokeSession (..),
    Session (..),
    UpdateIdentity (..),
    VerifiableAddress (..),
    Version (..),
    RecoveryAddress (..),
  )
where

import Pre

-- |
data Message = Message
  { -- |
    context :: Maybe Value,
    -- |
    id :: Maybe Integer,
    -- |
    text :: Maybe Text,
    -- | The flow type can either be `api` or `browser`.
    _type :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Message where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON Message where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- |
data ErrorContainer = ErrorContainer
  { -- | Errors in the container
    errors :: Value,
    -- |
    id :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ErrorContainer

instance ToJSON ErrorContainer where
  toEncoding = genericToEncoding defaultOptions

-- | Field represents a HTML Form Field
data FormField = FormField
  { -- | Disabled is the equivalent of `<input {{if .Disabled}}disabled{{end}}\">`
    disabled :: Maybe Bool,
    -- |
    messages :: Maybe [Message],
    -- | Name is the equivalent of `<input name=\"{{.Name}}\">`
    name :: Text,
    -- | Pattern is the equivalent of `<input pattern=\"{{.Pattern}}\">`
    p :: Maybe Text,
    -- | Required is the equivalent of `<input required=\"{{.Required}}\">`
    required :: Maybe Bool,
    -- | Type is the equivalent of `<input type=\"{{.Type}}\">`
    _type :: Text,
    -- | Value is the equivalent of `<input value=\"{{.Value}}\">`
    value :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON FormField where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON FormField where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- | Error responses are sent when an error (e.g. unauthorized, bad request, ...) occurred.
data GenericError = GenericError
  { -- |
    error :: Maybe GenericErrorPayload
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON GenericError

instance ToJSON GenericError where
  toEncoding = genericToEncoding defaultOptions

-- |
data GenericErrorPayload = GenericErrorPayload
  { -- | Code represents the error status code (404, 403, 401, ...).
    code :: Maybe Integer,
    -- | Debug contains debug information. This is usually not available and has to be enabled.
    debug :: Maybe Text,
    -- |
    details :: Maybe Value,
    -- |
    message :: Maybe Text,
    -- |
    reason :: Maybe Text,
    -- |
    request :: Maybe Text,
    -- |
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON GenericErrorPayload

instance ToJSON GenericErrorPayload where
  toEncoding = genericToEncoding defaultOptions

-- |
data HealthNotReadyStatus = HealthNotReadyStatus
  { -- | Errors contains a list of errors that caused the not ready status.
    errors :: Maybe (Map String Text)
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON HealthNotReadyStatus

instance ToJSON HealthNotReadyStatus where
  toEncoding = genericToEncoding defaultOptions

-- |
data HealthStatus = HealthStatus
  { -- | Status always contains \"ok\".
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON HealthStatus

instance ToJSON HealthStatus where
  toEncoding = genericToEncoding defaultOptions

-- |
data CompleteSelfServiceLoginFlowWithPasswordMethod = CompleteSelfServiceLoginFlowWithPasswordMethod
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Identifier is the email or username of the user trying to log in.
    identifier :: Maybe Text,
    -- | The user's password.
    password :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CompleteSelfServiceLoginFlowWithPasswordMethod

instance ToJSON CompleteSelfServiceLoginFlowWithPasswordMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data CompleteSelfServiceRecoveryFlowWithLinkMethod = CompleteSelfServiceRecoveryFlowWithLinkMethod
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Email to Recover  Needs to be set when initiating the flow. If the email is a registered recovery email, a recovery link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email in: body
    email :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CompleteSelfServiceRecoveryFlowWithLinkMethod

instance ToJSON CompleteSelfServiceRecoveryFlowWithLinkMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data CompleteSelfServiceSettingsFlowWithPasswordMethod = CompleteSelfServiceSettingsFlowWithPasswordMethod
  { -- | CSRFToken is the anti-CSRF token  type: string
    csrf_token :: Maybe Text,
    -- | Password is the updated password  type: string
    password :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CompleteSelfServiceSettingsFlowWithPasswordMethod

instance ToJSON CompleteSelfServiceSettingsFlowWithPasswordMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data CompleteSelfServiceVerificationFlowWithLinkMethod = CompleteSelfServiceVerificationFlowWithLinkMethod
  { -- | Sending the anti-csrf token is only required for browser login flows.
    csrf_token :: Maybe Text,
    -- | Email to Verify  Needs to be set when initiating the flow. If the email is a registered verification email, a verification link will be sent. If the email is not known, a email with details on what happened will be sent instead.  format: email in: body
    email :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CompleteSelfServiceVerificationFlowWithLinkMethod

instance ToJSON CompleteSelfServiceVerificationFlowWithLinkMethod where
  toEncoding = genericToEncoding defaultOptions

-- |
data CreateIdentity = CreateIdentity
  { -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
    schema_id :: Text,
    -- | Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_url`.
    traits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CreateIdentity

instance ToJSON CreateIdentity where
  toEncoding = genericToEncoding defaultOptions

-- |
data CreateRecoveryLink = CreateRecoveryLink
  { -- | Link Expires In  The recovery link will expire at that point in time. Defaults to the configuration value of `selfservice.flows.recovery.request_lifespan`.
    expires_in :: Maybe Text,
    -- |
    identity_id :: UUID
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON CreateRecoveryLink

instance ToJSON CreateRecoveryLink where
  toEncoding = genericToEncoding defaultOptions

-- |
data Identity = Identity
  { -- |
    id :: UUID,
    -- | RecoveryAddresses contains all the addresses that can be used to recover an identity.
    recovery_addresses :: Maybe [RecoveryAddress],
    -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
    schema_id :: Text,
    -- | SchemaURL is the URL of the endpoint where the identity's traits schema can be fetched from.  format: url
    schema_url :: Text,
    -- |
    traits :: Value,
    -- | VerifiableAddresses contains all the addresses that can be verified by the user.
    verifiable_addresses :: Maybe [VerifiableAddress]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Identity

instance ToJSON Identity where
  toEncoding = genericToEncoding defaultOptions

-- |
data RevokeSession = RevokeSession
  { -- | The Session Token  Invalidate this session token.
    session_token :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RevokeSession

instance ToJSON RevokeSession where
  toEncoding = genericToEncoding defaultOptions

-- |
data Session = Session
  { -- |
    active :: Maybe Bool,
    -- |
    authenticated_at :: UTCTime,
    -- |
    expires_at :: UTCTime,
    -- |
    id :: UUID,
    -- |
    identity :: Identity,
    -- |
    issued_at :: UTCTime
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Session

instance ToJSON Session where
  toEncoding = genericToEncoding defaultOptions

-- |
data UpdateIdentity = UpdateIdentity
  { -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits. If set will update the Identity's SchemaID.
    schema_id :: Maybe Text,
    -- | Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_id`.
    traits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UpdateIdentity

instance ToJSON UpdateIdentity where
  toEncoding = genericToEncoding defaultOptions

-- |
data VerifiableAddress = VerifiableAddress
  { -- |
    id :: UUID,
    -- |
    status :: Text,
    -- |
    value :: Text,
    -- |
    verified :: Bool,
    -- |
    verified_at :: Maybe UTCTime,
    -- |
    via :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerifiableAddress

instance ToJSON VerifiableAddress where
  toEncoding = genericToEncoding defaultOptions

-- |
data Version = Version
  { -- | Version is the service's version.
    version :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Version

instance ToJSON Version where
  toEncoding = genericToEncoding defaultOptions

-- |
data RecoveryAddress = RecoveryAddress
  { -- |
    id :: UUID,
    -- |
    value :: Text,
    -- |
    via :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON RecoveryAddress

instance ToJSON RecoveryAddress where
  toEncoding = genericToEncoding defaultOptions
module OryKratos.Types.Misc
  ( Message (..),
    ErrorContainer (..),
    FormField (..),
    GenericError (..),
    HealthNotReadyStatus (..),
    HealthStatus (..),
    CreateIdentity (..),
    CreateRecoveryLink (..),
    Identity (..),
    RevokeSession (..),
    Session (..),
    UpdateIdentity (..),
    VerifiableIdentityAddress (..),
    Version (..),
    RecoveryAddress (..),
    ContainerChangeResponseItem (..),
    ContainerCreateCreatedBody (..),
    ContainerTopOKBody (..),
    ContainerUpdateOKBody (..),
    ContainerWaitOKBody (..),
    ContainerWaitOKBodyError (..),
    ErrorResponse (..),
    GraphDriverData (..),
    IdResponse (..),
    ImageSummary (..),
    ImageDeleteResponseItem (..),
    JsonError (..),
    SelfServiceErrorContainer (..),
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
  { -- | The status code
    code :: Maybe Integer,
    -- | Debug information  This field is often not exposed to protect against leaking sensitive information.
    debug :: Maybe Text,
    -- | Further error details
    details :: Maybe Value,
    -- | Error message  The error's message.
    message :: Text,
    -- | A human-readable reason for the error
    reason :: Maybe Text,
    -- | The request ID  The request ID is often exposed internally in order to trace errors across service architectures. This is often a UUID.
    request :: Maybe Text,
    -- | The status description
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON GenericError

instance ToJSON GenericError where
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
data CreateRecoveryLink = AdminCreateSelfServiceRecoveryLinkBody
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
  { -- | CreatedAt is a helper struct field for gobuffalo.pop.
    created_at :: Maybe UTCTime,
    -- |
    id :: Text,
    -- | RecoveryAddresses contains all the addresses that can be used to recover an identity.
    recovery_addresses :: Maybe [RecoveryAddress],
    -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
    schema_id :: Text,
    -- | SchemaURL is the URL of the endpoint where the identity's traits schema can be fetched from.  format: url
    schema_url :: Text,
    -- | Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_url`.
    traits :: Value,
    -- | UpdatedAt is a helper struct field for gobuffalo.pop.
    updated_at :: Maybe UTCTime,
    -- | VerifiableAddresses contains all the addresses that can be verified by the user.
    verifiable_addresses :: Maybe [VerifiableIdentityAddress]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Identity

instance ToJSON Identity where
  toEncoding = genericToEncoding defaultOptions

-- | Credentials represents a specific credential type
data IdentityCredentials = IdentityCredentials
  { -- |
    config :: Maybe Value,
    -- | CreatedAt is a helper struct field for gobuffalo.pop.
    created_at :: Maybe UTCTime,
    -- | Identifiers represents a list of unique identifiers this credential type matches.
    identifiers :: Maybe [Text],
    -- | and so on.
    _type :: Maybe Text,
    -- | UpdatedAt is a helper struct field for gobuffalo.pop.
    updated_at :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentials where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON IdentityCredentials where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

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
    -- | State is the identity's state.
    state :: Value,
    -- | Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_id`.
    traits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UpdateIdentity

instance ToJSON UpdateIdentity where
  toEncoding = genericToEncoding defaultOptions

-- |
data VerifiableIdentityAddress = VerifiableIdentityAddress
  { -- | When this entry was created
    created_at :: Maybe UTCTime,
    -- |
    id :: UUID,
    -- | VerifiableAddressStatus must not exceed 16 characters as that is the limitation in the SQL Schema
    status :: Text,
    -- | When this entry was last updated
    updated_at :: Maybe UTCTime,
    -- | The address value  example foo@user.com
    value :: Text,
    -- | Indicates if the address has already been verified
    verified :: Bool,
    -- |
    verified_at :: Maybe UTCTime,
    -- | VerifiableAddressType must not exceed 16 characters as that is the limitation in the SQL Schema
    via :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON VerifiableIdentityAddress

instance ToJSON VerifiableIdentityAddress where
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

-- | ContainerChangeResponseItem change item in response to ContainerChanges operation
data ContainerChangeResponseItem = ContainerChangeResponseItem
  { -- | Kind of change
    item_kind :: Int,
    -- | Path to file that has changed
    item_path :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ContainerChangeResponseItem

instance ToJSON ContainerChangeResponseItem where
  toEncoding = genericToEncoding defaultOptions

-- | ContainerCreateCreatedBody OK response to ContainerCreate operation
data ContainerCreateCreatedBody = ContainerCreateCreatedBody
  { -- | The ID of the created container
    id :: Text,
    -- | Warnings encountered when creating the container
    warnings :: [Text]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ContainerCreateCreatedBody

instance ToJSON ContainerCreateCreatedBody where
  toEncoding = genericToEncoding defaultOptions

-- | ContainerTopOKBody OK response to ContainerTop operation
data ContainerTopOKBody = ContainerTopOKBody
  { -- | Each process running in the container, where each is process is an array of values corresponding to the titles
    processes :: [[Text]],
    -- | The ps column titles
    titles :: [Text]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ContainerTopOKBody

instance ToJSON ContainerTopOKBody where
  toEncoding = genericToEncoding defaultOptions

-- | ContainerUpdateOKBody OK response to ContainerUpdate operation
data ContainerUpdateOKBody = ContainerUpdateOKBody
  { -- | warnings
    warnings :: [Text]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ContainerUpdateOKBody

instance ToJSON ContainerUpdateOKBody where
  toEncoding = genericToEncoding defaultOptions

-- | ContainerWaitOKBody OK response to ContainerWait operation
data ContainerWaitOKBody = ContainerWaitOKBody
  { -- |
    error :: ContainerWaitOKBodyError,
    -- | Exit code of the container
    status_code :: Integer
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ContainerWaitOKBody

instance ToJSON ContainerWaitOKBody where
  toEncoding = genericToEncoding defaultOptions

-- | ContainerWaitOKBodyError container waiting error, if any
data ContainerWaitOKBodyError = ContainerWaitOKBodyError
  { -- | Details of an error
    error_message :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ContainerWaitOKBodyError

instance ToJSON ContainerWaitOKBodyError where
  toEncoding = genericToEncoding defaultOptions

-- |
data ErrorResponse = ErrorResponse
  { -- | The error message.
    message :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ErrorResponse

instance ToJSON ErrorResponse where
  toEncoding = genericToEncoding defaultOptions

data GraphDriverData = GraphDriverData
  { -- | data
    _data :: Map String Text,
    -- | name
    name :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON GraphDriverData where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON GraphDriverData where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- | IDResponse Response to an API call that returns just an Id
data IdResponse = IdResponse
  { -- | The id of the newly created object.
    id :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdResponse

instance ToJSON IdResponse where
  toEncoding = genericToEncoding defaultOptions

-- | ImageDeleteResponseItem image delete response item
data ImageDeleteResponseItem = ImageDeleteResponseItem
  { -- | The image ID of an image that was deleted
    deleted :: Maybe Text,
    -- | The image ID of an image that was untagged
    untagged :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ImageDeleteResponseItem

instance ToJSON ImageDeleteResponseItem where
  toEncoding = genericToEncoding defaultOptions

-- | ImageSummary image summary
data ImageSummary = ImageSummary
  { -- | containers
    containers :: Integer,
    -- | created
    created :: Integer,
    -- | Id
    id :: Text,
    -- | labels
    labels :: Map String Text,
    -- | parent Id
    parent_id :: Text,
    -- | repo digests
    repo_digests :: [Text],
    -- | repo tags
    repo_tags :: [Text],
    -- | shared size
    shared_size :: Integer,
    -- | size
    size :: Integer,
    -- | virtual size
    virtual_size :: Integer
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON ImageSummary

instance ToJSON ImageSummary where
  toEncoding = genericToEncoding defaultOptions

-- | The standard Ory JSON API error format.
data JsonError = JsonError
  { -- |
    error :: GenericError
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON JsonError

instance ToJSON JsonError where
  toEncoding = genericToEncoding defaultOptions

-- |
data SelfServiceErrorContainer = SelfServiceErrorContainer
  { -- | CreatedAt is a helper struct field for gobuffalo.pop.
    created_at :: Maybe UTCTime,
    -- | Errors in the container
    errors :: Value,
    -- |
    id :: Text,
    -- | UpdatedAt is a helper struct field for gobuffalo.pop.
    updated_at :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON SelfServiceErrorContainer

instance ToJSON SelfServiceErrorContainer where
  toEncoding = genericToEncoding defaultOptions

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types.Admin
  ( AdminCreateIdentityBody (..),
    AdminCreateIdentityImportCredentialsOidc (..),
    AdminCreateIdentityImportCredentialsOidcConfig (..),
    AdminCreateIdentityImportCredentialsOidcProvider (..),
    AdminCreateIdentityImportCredentialsPassword (..),
    AdminCreateIdentityImportCredentialsPasswordConfig (..),
    AdminCreateSelfServiceRecoveryLinkBody (..),
    AdminIdentityImportCredentials (..),
    AdminUpdateIdentityBody (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import qualified Data.Char as Char
import Data.Data (Data)
import Data.Function ((&))
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.UUID (UUID)
import GHC.Generics (Generic)
import OryKratos.Types.Helper (removeFieldLabelPrefix)
import OryKratos.Types.Identity (IdentityState)
import OryKratos.Types.Types
  ( RecoveryAddress,
    VerifiableIdentityAddress,
  )

data AdminCreateIdentityBody = AdminCreateIdentityBody
  { credentials :: Maybe AdminIdentityImportCredentials,
    -- | Store metadata about the user which is only accessible through admin APIs such as `GET /admin/identities/<id>`.
    metadata_admin :: Maybe Value,
    -- | Store metadata about the identity which the identity itself can see when calling for example the session endpoint. Do not store sensitive information (e.g. credit score) about the identity in this field.
    metadata_public :: Maybe Value,
    -- | RecoveryAddresses contains all the addresses that can be used to recover an identity.  Use this structure to import recovery addresses for an identity. Please keep in mind that the address needs to be represented in the Identity Schema or this field will be overwritten on the next identity update.
    recovery_addresses :: Maybe [RecoveryAddress],
    -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
    schema_id :: Text,
    state :: Maybe IdentityState,
    -- | Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_url`.
    traits :: Value,
    -- | VerifiableAddresses contains all the addresses that can be verified by the user.  Use this structure to import verified addresses for an identity. Please keep in mind that the address needs to be represented in the Identity Schema or this field will be overwritten on the next identity update.
    verifiable_addresses :: Maybe [VerifiableIdentityAddress]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityBody

instance ToJSON AdminCreateIdentityBody where
  toEncoding = genericToEncoding defaultOptions

data AdminCreateIdentityImportCredentialsOidc = AdminCreateIdentityImportCredentialsOidc
  { config :: Maybe AdminCreateIdentityImportCredentialsOidcConfig
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsOidc

instance ToJSON AdminCreateIdentityImportCredentialsOidc where
  toEncoding = genericToEncoding defaultOptions

data AdminCreateIdentityImportCredentialsOidcConfig = AdminCreateIdentityImportCredentialsOidcConfig
  { config :: Maybe AdminCreateIdentityImportCredentialsPasswordConfig,
    -- | A list of OpenID Connect Providers
    providers :: Maybe [AdminCreateIdentityImportCredentialsOidcProvider]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsOidcConfig

instance ToJSON AdminCreateIdentityImportCredentialsOidcConfig where
  toEncoding = genericToEncoding defaultOptions

data AdminCreateIdentityImportCredentialsOidcProvider = AdminCreateIdentityImportCredentialsOidcProvider
  { -- | The OpenID Connect provider to link the subject to. Usually something like `google` or `github`.
    provider :: Text,
    -- | The subject (`sub`) of the OpenID Connect connection. Usually the `sub` field of the ID Token.
    subject :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsOidcProvider

instance ToJSON AdminCreateIdentityImportCredentialsOidcProvider where
  toEncoding = genericToEncoding defaultOptions

data AdminCreateIdentityImportCredentialsPassword = AdminCreateIdentityImportCredentialsPassword
  { config :: Maybe AdminCreateIdentityImportCredentialsPasswordConfig
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsPassword

instance ToJSON AdminCreateIdentityImportCredentialsPassword where
  toEncoding = genericToEncoding defaultOptions

data AdminCreateIdentityImportCredentialsPasswordConfig = AdminCreateIdentityImportCredentialsPasswordConfig
  { -- | The hashed password in [PHC format]( https://www.ory.sh/docs/kratos/concepts/credentials/username-email-password#hashed-password-format)
    hashed_password :: Maybe Text,
    -- | The password in plain text if no hash is available.
    password :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsPasswordConfig

instance ToJSON AdminCreateIdentityImportCredentialsPasswordConfig where
  toEncoding = genericToEncoding defaultOptions

data AdminCreateSelfServiceRecoveryLinkBody = AdminCreateSelfServiceRecoveryLinkBody
  { -- | Link Expires In  The recovery link will expire at that point in time. Defaults to the configuration value of `selfservice.flows.recovery.request_lifespan`.
    expires_in :: Maybe Text,
    identity_id :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateSelfServiceRecoveryLinkBody

instance ToJSON AdminCreateSelfServiceRecoveryLinkBody where
  toEncoding = genericToEncoding defaultOptions

data AdminIdentityImportCredentials = AdminIdentityImportCredentials
  { oidc :: Maybe AdminCreateIdentityImportCredentialsOidc,
    password :: Maybe AdminCreateIdentityImportCredentialsPassword
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminIdentityImportCredentials

instance ToJSON AdminIdentityImportCredentials where
  toEncoding = genericToEncoding defaultOptions

data AdminUpdateIdentityBody = AdminUpdateIdentityBody
  { -- | Store metadata about the user which is only accessible through admin APIs such as `GET /admin/identities/<id>`.
    metadata_admin :: Maybe Value,
    -- | Store metadata about the identity which the identity itself can see when calling for example the session endpoint. Do not store sensitive information (e.g. credit score) about the identity in this field.
    metadata_public :: Maybe Value,
    -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits. If set will update the Identity's SchemaID.
    schema_id :: Text,
    state :: IdentityState,
    -- | Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_id`.
    traits :: Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminUpdateIdentityBody

instance ToJSON AdminUpdateIdentityBody where
  toEncoding = genericToEncoding defaultOptions

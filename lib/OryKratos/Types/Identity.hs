{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types.Identity
  ( Identity (..),
    IdentityCredentials (..),
    IdentityCredentialsOidc (..),
    IdentityCredentialsOidcProvider (..),
    IdentityCredentialsPassword (..),
    IdentityCredentialsType (..),
    IdentitySchema (..),
    IdentityState (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, genericParseJSON, genericToEncoding, genericToJSON)
import qualified Data.Aeson as Aeson
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
import OryKratos.Types.Helper (customOptions, removeFieldLabelPrefix)
import OryKratos.Types.Types
  ( RecoveryAddress,
    VerifiableIdentityAddress,
  )

-- | An identity can be a real human, a service, an IoT device - everything that can be described as an \&quot;actor\&quot; in a system.
data Identity traits = Identity
  { -- | CreatedAt is a helper struct field for gobuffalo.pop.
    created_at :: Maybe UTCTime,
    -- | Credentials represents all credentials that can be used for authenticating this identity.
    credentials :: Maybe (Map.Map String IdentityCredentials),
    id :: UUID,
    -- | NullJSONRawMessage represents a json.RawMessage that works well with JSON, SQL, and Swagger and is NULLable-
    metadata_admin :: Maybe Value,
    -- | NullJSONRawMessage represents a json.RawMessage that works well with JSON, SQL, and Swagger and is NULLable-
    metadata_public :: Maybe Value,
    -- | RecoveryAddresses contains all the addresses that can be used to recover an identity.
    recovery_addresses :: Maybe [RecoveryAddress],
    -- | SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
    schema_id :: Text,
    -- | SchemaURL is the URL of the endpoint where the identity's traits schema can be fetched from.  format: url
    schema_url :: Text,
    state :: Maybe IdentityState,
    state_changed_at :: Maybe UTCTime,
    -- | Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_url`.
    traits :: traits,
    -- | UpdatedAt is a helper struct field for gobuffalo.pop.
    updated_at :: Maybe UTCTime,
    -- | VerifiableAddresses contains all the addresses that can be verified by the user.
    verifiable_addresses :: Maybe [VerifiableIdentityAddress]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON traits => FromJSON (Identity traits)

instance ToJSON traits => ToJSON (Identity traits) where
  toEncoding = genericToEncoding defaultOptions

-- | Credentials represents a specific credential type
data IdentityCredentials = IdentityCredentials
  { config :: Maybe Value,
    -- | CreatedAt is a helper struct field for gobuffalo.pop.
    created_at :: Maybe UTCTime,
    -- | Identifiers represents a list of unique identifiers this credential type matches.
    identifiers :: Maybe [Text],
    _type :: Maybe IdentityCredentialsType,
    -- | UpdatedAt is a helper struct field for gobuffalo.pop.
    updated_at :: Maybe UTCTime,
    -- | Version refers to the version of the credential. Useful when changing the config schema.
    version :: Maybe Integer
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentials where
  parseJSON = genericParseJSON customOptions

instance ToJSON IdentityCredentials where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data IdentityCredentialsOidc = IdentityCredentialsOidc
  { providers :: Maybe [IdentityCredentialsOidcProvider]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsOidc

instance ToJSON IdentityCredentialsOidc where
  toEncoding = genericToEncoding defaultOptions

data IdentityCredentialsOidcProvider = IdentityCredentialsOidcProvider
  { initial_access_token :: Maybe Text,
    initial_id_token :: Maybe Text,
    initial_refresh_token :: Maybe Text,
    provider :: Maybe Text,
    subject :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsOidcProvider

instance ToJSON IdentityCredentialsOidcProvider where
  toEncoding = genericToEncoding defaultOptions

data IdentityCredentialsPassword = IdentityCredentialsPassword
  { -- | HashedPassword is a hash-representation of the password.
    hashed_password :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsPassword

instance ToJSON IdentityCredentialsPassword where
  toEncoding = genericToEncoding defaultOptions

-- | and so on.
data IdentityCredentialsType
  = Password
  | TOTP
  | OIDC
  | WebAuthn
  | LookupSecret
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsType where
  parseJSON (Aeson.String s) = case T.unpack s of
    "password" -> return Password
    "totp" -> return TOTP
    "oidc" -> return OIDC
    "webauthn" -> return WebAuthn
    "lookup_secret" -> return LookupSecret
    _ -> error "Invalid IdentityCredentialsType"
  parseJSON _ = error "Invalid IdentityCredentialsType"

instance ToJSON IdentityCredentialsType where
  toJSON (Password) = Aeson.String "password"
  toJSON (TOTP) = Aeson.String "totp"
  toJSON (OIDC) = Aeson.String "oidc"
  toJSON (WebAuthn) = Aeson.String "webauthn"
  toJSON (LookupSecret) = Aeson.String "lookup_secret"

data IdentitySchema = IdentitySchema
  { -- | The ID of the Identity JSON Schema
    id :: Maybe Text,
    -- | The actual Identity JSON Schema
    schema :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentitySchema

instance ToJSON IdentitySchema where
  toEncoding = genericToEncoding defaultOptions

-- | The state can either be &#x60;active&#x60; or &#x60;inactive&#x60;.
data IdentityState = IdentityStateActive | IdentityStateInactive deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityState where
  parseJSON (Aeson.String s) = case T.unpack s of
    "active" -> return IdentityStateActive
    "inactive" -> return IdentityStateInactive
    _ -> error "Invalid IdentityState"
  parseJSON _ = error "Invalid IdentityState"

instance ToJSON IdentityState where
  toJSON (IdentityStateActive) = Aeson.String "active"
  toJSON (IdentityStateInactive) = Aeson.String "inactive"

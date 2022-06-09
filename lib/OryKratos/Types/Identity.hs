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

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON, genericToEncoding)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))
import OryKratos.Types.Types
    ( VerifiableIdentityAddress, RecoveryAddress )
import OryKratos.Types.Helper ( removeFieldLabelPrefix )

-- | An identity can be a real human, a service, an IoT device - everything that can be described as an \&quot;actor\&quot; in a system.
data Identity = Identity
  { identityCreatedUnderscoreat :: Maybe UTCTime -- ^ CreatedAt is a helper struct field for gobuffalo.pop.
  , identityCredentials :: Maybe (Map.Map String IdentityCredentials) -- ^ Credentials represents all credentials that can be used for authenticating this identity.
  , identityId :: Text -- ^ 
  , identityMetadataUnderscoreadmin :: Maybe Value -- ^ NullJSONRawMessage represents a json.RawMessage that works well with JSON, SQL, and Swagger and is NULLable-
  , identityMetadataUnderscorepublic :: Maybe Value -- ^ NullJSONRawMessage represents a json.RawMessage that works well with JSON, SQL, and Swagger and is NULLable-
  , identityRecoveryUnderscoreaddresses :: Maybe [RecoveryAddress] -- ^ RecoveryAddresses contains all the addresses that can be used to recover an identity.
  , identitySchemaUnderscoreid :: Text -- ^ SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
  , identitySchemaUnderscoreurl :: Text -- ^ SchemaURL is the URL of the endpoint where the identity's traits schema can be fetched from.  format: url
  , identityState :: Maybe IdentityState -- ^ 
  , identityStateUnderscorechangedUnderscoreat :: Maybe UTCTime -- ^ 
  , identityTraits :: Value -- ^ Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_url`.
  , identityUpdatedUnderscoreat :: Maybe UTCTime -- ^ UpdatedAt is a helper struct field for gobuffalo.pop.
  , identityVerifiableUnderscoreaddresses :: Maybe [VerifiableIdentityAddress] -- ^ VerifiableAddresses contains all the addresses that can be verified by the user.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON Identity where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identity")
instance ToJSON Identity where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identity")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "identity")


-- | Credentials represents a specific credential type
data IdentityCredentials = IdentityCredentials
  { identityCredentialsConfig :: Maybe Value -- ^ 
  , identityCredentialsCreatedUnderscoreat :: Maybe UTCTime -- ^ CreatedAt is a helper struct field for gobuffalo.pop.
  , identityCredentialsIdentifiers :: Maybe [Text] -- ^ Identifiers represents a list of unique identifiers this credential type matches.
  , identityCredentialsType :: Maybe IdentityCredentialsType -- ^ 
  , identityCredentialsUpdatedUnderscoreat :: Maybe UTCTime -- ^ UpdatedAt is a helper struct field for gobuffalo.pop.
  , identityCredentialsVersion :: Maybe Integer -- ^ Version refers to the version of the credential. Useful when changing the config schema.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentials where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentials")
instance ToJSON IdentityCredentials where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentials")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "identityCredentials")


-- | 
data IdentityCredentialsOidc = IdentityCredentialsOidc
  { identityCredentialsOidcProviders :: Maybe [IdentityCredentialsOidcProvider] -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsOidc where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentialsOidc")
instance ToJSON IdentityCredentialsOidc where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentialsOidc")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "identityCredentialsOidc")


-- | 
data IdentityCredentialsOidcProvider = IdentityCredentialsOidcProvider
  { identityCredentialsOidcProviderInitialUnderscoreaccessUnderscoretoken :: Maybe Text -- ^ 
  , identityCredentialsOidcProviderInitialUnderscoreidUnderscoretoken :: Maybe Text -- ^ 
  , identityCredentialsOidcProviderInitialUnderscorerefreshUnderscoretoken :: Maybe Text -- ^ 
  , identityCredentialsOidcProviderProvider :: Maybe Text -- ^ 
  , identityCredentialsOidcProviderSubject :: Maybe Text -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsOidcProvider where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentialsOidcProvider")
instance ToJSON IdentityCredentialsOidcProvider where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentialsOidcProvider")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "identityCredentialsOidcProvider")


-- | 
data IdentityCredentialsPassword = IdentityCredentialsPassword
  { identityCredentialsPasswordHashedUnderscorepassword :: Maybe Text -- ^ HashedPassword is a hash-representation of the password.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsPassword where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentialsPassword")
instance ToJSON IdentityCredentialsPassword where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentialsPassword")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "identityCredentialsPassword")


-- | and so on.
data IdentityCredentialsType = IdentityCredentialsType
  { 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityCredentialsType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityCredentialsType")
instance ToJSON IdentityCredentialsType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityCredentialsType")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "identityCredentialsType")


-- | 
data IdentitySchema = IdentitySchema
  { identitySchemaId :: Maybe Text -- ^ The ID of the Identity JSON Schema
  , identitySchemaSchema :: Maybe Value -- ^ The actual Identity JSON Schema
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentitySchema where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identitySchema")
instance ToJSON IdentitySchema where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identitySchema")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "identitySchema")


-- | The state can either be &#x60;active&#x60; or &#x60;inactive&#x60;.
data IdentityState = IdentityState
  { 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON IdentityState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "identityState")
instance ToJSON IdentityState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "identityState")
  toEncoding = genericToEncoding (removeFieldLabelPrefix False "identityState")

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

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
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
    ( VerifiableIdentityAddress,
      RecoveryAddress,
       )
import OryKratos.Types.Identity (IdentityState)
import OryKratos.Types.Helper ( removeFieldLabelPrefix )


-- | 
data AdminCreateIdentityBody = AdminCreateIdentityBody
  { adminCreateIdentityBodyCredentials :: Maybe AdminIdentityImportCredentials -- ^ 
  , adminCreateIdentityBodyMetadataUnderscoreadmin :: Maybe Value -- ^ Store metadata about the user which is only accessible through admin APIs such as `GET /admin/identities/<id>`.
  , adminCreateIdentityBodyMetadataUnderscorepublic :: Maybe Value -- ^ Store metadata about the identity which the identity itself can see when calling for example the session endpoint. Do not store sensitive information (e.g. credit score) about the identity in this field.
  , adminCreateIdentityBodyRecoveryUnderscoreaddresses :: Maybe [RecoveryAddress] -- ^ RecoveryAddresses contains all the addresses that can be used to recover an identity.  Use this structure to import recovery addresses for an identity. Please keep in mind that the address needs to be represented in the Identity Schema or this field will be overwritten on the next identity update.
  , adminCreateIdentityBodySchemaUnderscoreid :: Text -- ^ SchemaID is the ID of the JSON Schema to be used for validating the identity's traits.
  , adminCreateIdentityBodyState :: Maybe IdentityState -- ^ 
  , adminCreateIdentityBodyTraits :: Value -- ^ Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_url`.
  , adminCreateIdentityBodyVerifiableUnderscoreaddresses :: Maybe [VerifiableIdentityAddress] -- ^ VerifiableAddresses contains all the addresses that can be verified by the user.  Use this structure to import verified addresses for an identity. Please keep in mind that the address needs to be represented in the Identity Schema or this field will be overwritten on the next identity update.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityBody")
instance ToJSON AdminCreateIdentityBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityBody")


-- | 
data AdminCreateIdentityImportCredentialsOidc = AdminCreateIdentityImportCredentialsOidc
  { adminCreateIdentityImportCredentialsOidcConfig :: Maybe AdminCreateIdentityImportCredentialsOidcConfig -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsOidc where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsOidc")
instance ToJSON AdminCreateIdentityImportCredentialsOidc where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsOidc")


-- | 
data AdminCreateIdentityImportCredentialsOidcConfig = AdminCreateIdentityImportCredentialsOidcConfig
  { adminCreateIdentityImportCredentialsOidcConfigConfig :: Maybe AdminCreateIdentityImportCredentialsPasswordConfig -- ^ 
  , adminCreateIdentityImportCredentialsOidcConfigProviders :: Maybe [AdminCreateIdentityImportCredentialsOidcProvider] -- ^ A list of OpenID Connect Providers
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsOidcConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsOidcConfig")
instance ToJSON AdminCreateIdentityImportCredentialsOidcConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsOidcConfig")


-- | 
data AdminCreateIdentityImportCredentialsOidcProvider = AdminCreateIdentityImportCredentialsOidcProvider
  { adminCreateIdentityImportCredentialsOidcProviderProvider :: Text -- ^ The OpenID Connect provider to link the subject to. Usually something like `google` or `github`.
  , adminCreateIdentityImportCredentialsOidcProviderSubject :: Text -- ^ The subject (`sub`) of the OpenID Connect connection. Usually the `sub` field of the ID Token.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsOidcProvider where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsOidcProvider")
instance ToJSON AdminCreateIdentityImportCredentialsOidcProvider where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsOidcProvider")


-- | 
data AdminCreateIdentityImportCredentialsPassword = AdminCreateIdentityImportCredentialsPassword
  { adminCreateIdentityImportCredentialsPasswordConfig :: Maybe AdminCreateIdentityImportCredentialsPasswordConfig -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsPassword where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsPassword")
instance ToJSON AdminCreateIdentityImportCredentialsPassword where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsPassword")


-- | 
data AdminCreateIdentityImportCredentialsPasswordConfig = AdminCreateIdentityImportCredentialsPasswordConfig
  { adminCreateIdentityImportCredentialsPasswordConfigHashedUnderscorepassword :: Maybe Text -- ^ The hashed password in [PHC format]( https://www.ory.sh/docs/kratos/concepts/credentials/username-email-password#hashed-password-format)
  , adminCreateIdentityImportCredentialsPasswordConfigPassword :: Maybe Text -- ^ The password in plain text if no hash is available.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateIdentityImportCredentialsPasswordConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateIdentityImportCredentialsPasswordConfig")
instance ToJSON AdminCreateIdentityImportCredentialsPasswordConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateIdentityImportCredentialsPasswordConfig")


-- | 
data AdminCreateSelfServiceRecoveryLinkBody = AdminCreateSelfServiceRecoveryLinkBody
  { adminCreateSelfServiceRecoveryLinkBodyExpiresUnderscorein :: Maybe Text -- ^ Link Expires In  The recovery link will expire at that point in time. Defaults to the configuration value of `selfservice.flows.recovery.request_lifespan`.
  , adminCreateSelfServiceRecoveryLinkBodyIdentityUnderscoreid :: Text -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminCreateSelfServiceRecoveryLinkBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminCreateSelfServiceRecoveryLinkBody")
instance ToJSON AdminCreateSelfServiceRecoveryLinkBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminCreateSelfServiceRecoveryLinkBody")


-- | 
data AdminIdentityImportCredentials = AdminIdentityImportCredentials
  { adminIdentityImportCredentialsOidc :: Maybe AdminCreateIdentityImportCredentialsOidc -- ^ 
  , adminIdentityImportCredentialsPassword :: Maybe AdminCreateIdentityImportCredentialsPassword -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminIdentityImportCredentials where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminIdentityImportCredentials")
instance ToJSON AdminIdentityImportCredentials where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminIdentityImportCredentials")


-- | 
data AdminUpdateIdentityBody = AdminUpdateIdentityBody
  { adminUpdateIdentityBodyMetadataUnderscoreadmin :: Maybe Value -- ^ Store metadata about the user which is only accessible through admin APIs such as `GET /admin/identities/<id>`.
  , adminUpdateIdentityBodyMetadataUnderscorepublic :: Maybe Value -- ^ Store metadata about the identity which the identity itself can see when calling for example the session endpoint. Do not store sensitive information (e.g. credit score) about the identity in this field.
  , adminUpdateIdentityBodySchemaUnderscoreid :: Text -- ^ SchemaID is the ID of the JSON Schema to be used for validating the identity's traits. If set will update the Identity's SchemaID.
  , adminUpdateIdentityBodyState :: IdentityState -- ^ 
  , adminUpdateIdentityBodyTraits :: Value -- ^ Traits represent an identity's traits. The identity is able to create, modify, and delete traits in a self-service manner. The input will always be validated against the JSON Schema defined in `schema_id`.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON AdminUpdateIdentityBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminUpdateIdentityBody")
instance ToJSON AdminUpdateIdentityBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminUpdateIdentityBody")


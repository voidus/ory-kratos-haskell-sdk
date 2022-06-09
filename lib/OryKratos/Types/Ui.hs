{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OryKratos.Types.Ui
  ( UiContainer (..),
    UiNode (..),
    UiNodeAnchorAttributes (..),
    UiNodeAttributes (..),
    UiNodeImageAttributes (..),
    UiNodeInputAttributes (..),
    UiNodeMeta (..),
    UiNodeScriptAttributes (..),
    UiNodeTextAttributes (..),
    UiText (..),
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
import OryKratos.Types.Helper (customOptions, removeFieldLabelPrefix)

-- | Container represents a HTML Form. The container can work with both HTTP Form and JSON requests
data UiContainer = UiContainer
  { -- | Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
    action :: Text,
    messages :: Maybe [UiText],
    -- | Method is the form method (e.g. POST)
    method :: Text,
    nodes :: [UiNode]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiContainer

instance ToJSON UiContainer where
  toEncoding = genericToEncoding defaultOptions

-- | Nodes are represented as HTML elements or their native UI equivalents. For example, a node can be an &#x60;&lt;img&gt;&#x60; tag, or an &#x60;&lt;input element&gt;&#x60; but also &#x60;some plain text&#x60;.
data UiNode = UiNode
  { attributes :: UiNodeAttributes,
    -- | Group specifies which group (e.g. password authenticator) this node belongs to.
    group :: Text,
    messages :: [UiText],
    meta :: UiNodeMeta,
    -- | The node's type
    _type :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNode where
  parseJSON = genericParseJSON customOptions

instance ToJSON UiNode where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data UiNodeAnchorAttributes = UiNodeAnchorAttributes
  { -- | The link's href (destination) URL.  format: uri
    href :: Text,
    -- | A unique identifier
    id :: Text,
    -- | NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"a\".
    node_type :: Text,
    title :: UiText
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeAnchorAttributes

instance ToJSON UiNodeAnchorAttributes where
  toEncoding = genericToEncoding defaultOptions

data UiNodeAttributes = UiNodeAttributes
  { -- | Sets the input's disabled field to true or false.
    disabled :: Bool,
    label :: Maybe UiText,
    -- | The input's element name.
    name :: Text,
    -- | NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0. In this struct it technically always is \"script\".
    node_type :: Text,
    -- | OnClick may contain javascript which should be executed on click. This is primarily used for WebAuthn.
    onclick :: Maybe Text,
    -- | The input's pattern.
    _pattern :: Maybe Text,
    -- | Mark this input field as required.
    required :: Maybe Bool,
    -- | The script MIME type
    _type :: Text,
    -- | The input's value.
    value :: Maybe Value,
    -- | A unique identifier
    id :: Text,
    text :: UiText,
    -- | Height of the image
    height :: Integer,
    -- | The script source
    src :: Text,
    -- | Width of the image
    width :: Integer,
    -- | The link's href (destination) URL.  format: uri
    href :: Text,
    title :: UiText,
    -- | The script async type
    async :: Bool,
    -- | The script cross origin policy
    crossorigin :: Text,
    -- | The script's integrity hash
    integrity :: Text,
    -- | Nonce for CSP  A nonce you may want to use to improve your Content Security Policy. You do not have to use this value but if you want to improve your CSP policies you may use it. You can also choose to use your own nonce value!
    nonce :: Text,
    -- | The script referrer policy
    referrerpolicy :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeAttributes where
  parseJSON = genericParseJSON customOptions

instance ToJSON UiNodeAttributes where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data UiNodeImageAttributes = UiNodeImageAttributes
  { -- | Height of the image
    height :: Integer,
    -- | A unique identifier
    id :: Text,
    -- | NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"img\".
    node_type :: Text,
    -- | The image's source URL.  format: uri
    src :: Text,
    -- | Width of the image
    width :: Integer
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeImageAttributes

instance ToJSON UiNodeImageAttributes where
  toEncoding = genericToEncoding defaultOptions

-- | InputAttributes represents the attributes of an input node
data UiNodeInputAttributes = UiNodeInputAttributes
  { -- | Sets the input's disabled field to true or false.
    disabled :: Bool,
    label :: Maybe UiText,
    -- | The input's element name.
    name :: Text,
    -- | NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"input\".
    node_type :: Text,
    -- | OnClick may contain javascript which should be executed on click. This is primarily used for WebAuthn.
    onclick :: Maybe Text,
    -- | The input's pattern.
    _pattern :: Maybe Text,
    -- | Mark this input field as required.
    required :: Maybe Bool,
    _type :: Text,
    -- | The input's value.
    value :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeInputAttributes where
  parseJSON = genericParseJSON customOptions

instance ToJSON UiNodeInputAttributes where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

-- | This might include a label and other information that can optionally be used to render UIs.
data UiNodeMeta = UiNodeMeta
  { label :: Maybe UiText
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeMeta

instance ToJSON UiNodeMeta where
  toEncoding = genericToEncoding defaultOptions

data UiNodeScriptAttributes = UiNodeScriptAttributes
  { -- | The script async type
    async :: Bool,
    -- | The script cross origin policy
    crossorigin :: Text,
    -- | A unique identifier
    id :: Text,
    -- | The script's integrity hash
    integrity :: Text,
    -- | NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0. In this struct it technically always is \"script\".
    node_type :: Text,
    -- | Nonce for CSP  A nonce you may want to use to improve your Content Security Policy. You do not have to use this value but if you want to improve your CSP policies you may use it. You can also choose to use your own nonce value!
    nonce :: Text,
    -- | The script referrer policy
    referrerpolicy :: Text,
    -- | The script source
    src :: Text,
    -- | The script MIME type
    _type :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeScriptAttributes where
  parseJSON = genericParseJSON customOptions

instance ToJSON UiNodeScriptAttributes where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data UiNodeTextAttributes = UiNodeTextAttributes
  { -- | A unique identifier
    id :: Text,
    -- | NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"text\".
    node_type :: Text,
    text :: UiText
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeTextAttributes

instance ToJSON UiNodeTextAttributes where
  toEncoding = genericToEncoding defaultOptions

data UiText = UiText
  { -- | The message's context. Useful when customizing messages.
    context :: Maybe Value,
    id :: Integer,
    -- | The message text. Written in american english.
    text :: Text,
    _type :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiText where
  parseJSON = genericParseJSON customOptions

instance ToJSON UiText where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

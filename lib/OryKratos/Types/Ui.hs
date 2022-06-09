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
import OryKratos.Types.Helper ( removeFieldLabelPrefix )


-- | Container represents a HTML Form. The container can work with both HTTP Form and JSON requests
data UiContainer = UiContainer
  { uiContainerAction :: Text -- ^ Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
  , uiContainerMessages :: Maybe [UiText] -- ^ 
  , uiContainerMethod :: Text -- ^ Method is the form method (e.g. POST)
  , uiContainerNodes :: [UiNode] -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiContainer where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiContainer")
instance ToJSON UiContainer where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiContainer")


-- | Nodes are represented as HTML elements or their native UI equivalents. For example, a node can be an &#x60;&lt;img&gt;&#x60; tag, or an &#x60;&lt;input element&gt;&#x60; but also &#x60;some plain text&#x60;.
data UiNode = UiNode
  { uiNodeAttributes :: UiNodeAttributes -- ^ 
  , uiNodeGroup :: Text -- ^ Group specifies which group (e.g. password authenticator) this node belongs to.
  , uiNodeMessages :: [UiText] -- ^ 
  , uiNodeMeta :: UiNodeMeta -- ^ 
  , uiNodeType :: Text -- ^ The node's type
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNode where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNode")
instance ToJSON UiNode where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNode")


-- | 
data UiNodeAnchorAttributes = UiNodeAnchorAttributes
  { uiNodeAnchorAttributesHref :: Text -- ^ The link's href (destination) URL.  format: uri
  , uiNodeAnchorAttributesId :: Text -- ^ A unique identifier
  , uiNodeAnchorAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"a\".
  , uiNodeAnchorAttributesTitle :: UiText -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeAnchorAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeAnchorAttributes")
instance ToJSON UiNodeAnchorAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeAnchorAttributes")


-- | 
data UiNodeAttributes = UiNodeAttributes
  { uiNodeAttributesDisabled :: Bool -- ^ Sets the input's disabled field to true or false.
  , uiNodeAttributesLabel :: Maybe UiText -- ^ 
  , uiNodeAttributesName :: Text -- ^ The input's element name.
  , uiNodeAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0. In this struct it technically always is \"script\".
  , uiNodeAttributesOnclick :: Maybe Text -- ^ OnClick may contain javascript which should be executed on click. This is primarily used for WebAuthn.
  , uiNodeAttributesPattern :: Maybe Text -- ^ The input's pattern.
  , uiNodeAttributesRequired :: Maybe Bool -- ^ Mark this input field as required.
  , uiNodeAttributesType :: Text -- ^ The script MIME type
  , uiNodeAttributesValue :: Maybe Value -- ^ The input's value.
  , uiNodeAttributesId :: Text -- ^ A unique identifier
  , uiNodeAttributesText :: UiText -- ^ 
  , uiNodeAttributesHeight :: Integer -- ^ Height of the image
  , uiNodeAttributesSrc :: Text -- ^ The script source
  , uiNodeAttributesWidth :: Integer -- ^ Width of the image
  , uiNodeAttributesHref :: Text -- ^ The link's href (destination) URL.  format: uri
  , uiNodeAttributesTitle :: UiText -- ^ 
  , uiNodeAttributesAsync :: Bool -- ^ The script async type
  , uiNodeAttributesCrossorigin :: Text -- ^ The script cross origin policy
  , uiNodeAttributesIntegrity :: Text -- ^ The script's integrity hash
  , uiNodeAttributesNonce :: Text -- ^ Nonce for CSP  A nonce you may want to use to improve your Content Security Policy. You do not have to use this value but if you want to improve your CSP policies you may use it. You can also choose to use your own nonce value!
  , uiNodeAttributesReferrerpolicy :: Text -- ^ The script referrer policy
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeAttributes")
instance ToJSON UiNodeAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeAttributes")


-- | 
data UiNodeImageAttributes = UiNodeImageAttributes
  { uiNodeImageAttributesHeight :: Integer -- ^ Height of the image
  , uiNodeImageAttributesId :: Text -- ^ A unique identifier
  , uiNodeImageAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"img\".
  , uiNodeImageAttributesSrc :: Text -- ^ The image's source URL.  format: uri
  , uiNodeImageAttributesWidth :: Integer -- ^ Width of the image
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeImageAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeImageAttributes")
instance ToJSON UiNodeImageAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeImageAttributes")


-- | InputAttributes represents the attributes of an input node
data UiNodeInputAttributes = UiNodeInputAttributes
  { uiNodeInputAttributesDisabled :: Bool -- ^ Sets the input's disabled field to true or false.
  , uiNodeInputAttributesLabel :: Maybe UiText -- ^ 
  , uiNodeInputAttributesName :: Text -- ^ The input's element name.
  , uiNodeInputAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"input\".
  , uiNodeInputAttributesOnclick :: Maybe Text -- ^ OnClick may contain javascript which should be executed on click. This is primarily used for WebAuthn.
  , uiNodeInputAttributesPattern :: Maybe Text -- ^ The input's pattern.
  , uiNodeInputAttributesRequired :: Maybe Bool -- ^ Mark this input field as required.
  , uiNodeInputAttributesType :: Text -- ^ 
  , uiNodeInputAttributesValue :: Maybe Value -- ^ The input's value.
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeInputAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeInputAttributes")
instance ToJSON UiNodeInputAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeInputAttributes")


-- | This might include a label and other information that can optionally be used to render UIs.
data UiNodeMeta = UiNodeMeta
  { uiNodeMetaLabel :: Maybe UiText -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeMeta where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeMeta")
instance ToJSON UiNodeMeta where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeMeta")


-- | 
data UiNodeScriptAttributes = UiNodeScriptAttributes
  { uiNodeScriptAttributesAsync :: Bool -- ^ The script async type
  , uiNodeScriptAttributesCrossorigin :: Text -- ^ The script cross origin policy
  , uiNodeScriptAttributesId :: Text -- ^ A unique identifier
  , uiNodeScriptAttributesIntegrity :: Text -- ^ The script's integrity hash
  , uiNodeScriptAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0. In this struct it technically always is \"script\".
  , uiNodeScriptAttributesNonce :: Text -- ^ Nonce for CSP  A nonce you may want to use to improve your Content Security Policy. You do not have to use this value but if you want to improve your CSP policies you may use it. You can also choose to use your own nonce value!
  , uiNodeScriptAttributesReferrerpolicy :: Text -- ^ The script referrer policy
  , uiNodeScriptAttributesSrc :: Text -- ^ The script source
  , uiNodeScriptAttributesType :: Text -- ^ The script MIME type
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeScriptAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeScriptAttributes")
instance ToJSON UiNodeScriptAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeScriptAttributes")


-- | 
data UiNodeTextAttributes = UiNodeTextAttributes
  { uiNodeTextAttributesId :: Text -- ^ A unique identifier
  , uiNodeTextAttributesNodeUnderscoretype :: Text -- ^ NodeType represents this node's types. It is a mirror of `node.type` and is primarily used to allow compatibility with OpenAPI 3.0.  In this struct it technically always is \"text\".
  , uiNodeTextAttributesText :: UiText -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeTextAttributes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiNodeTextAttributes")
instance ToJSON UiNodeTextAttributes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiNodeTextAttributes")


-- | 
data UiText = UiText
  { uiTextContext :: Maybe Value -- ^ The message's context. Useful when customizing messages.
  , uiTextId :: Integer -- ^ 
  , uiTextText :: Text -- ^ The message text. Written in american english.
  , uiTextType :: Text -- ^ 
  } deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiText where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uiText")
instance ToJSON UiText where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uiText")

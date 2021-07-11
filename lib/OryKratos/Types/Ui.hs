module OryKratos.Types.Ui
  ( UiContainer (..),
    UiNode (..),
    UiNodeAnchorAttributes (..),
    UiNodeImageAttributes (..),
    UiNodeInputAttributes (..),
    UiNodeTextAttributes (..),
    UiText (..),
    Meta (..),
  )
where

import Pre

-- | Container represents a HTML Form. The container can work with both HTTP Form and JSON requests
data UiContainer = UiContainer
  { -- | Action should be used as the form action URL `<form action=\"{{ .Action }}\" method=\"post\">`.
    action :: Text,
    -- |
    messages :: Maybe [UiText],
    -- | Method is the form method (e.g. POST)
    method :: Text,
    -- |
    nodes :: [UiNode]
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiContainer

instance ToJSON UiContainer where
  toEncoding = genericToEncoding defaultOptions

-- | Nodes are represented as HTML elements or their native UI equivalents. For example, a node can be an &#x60;&lt;img&gt;&#x60; tag, or an &#x60;&lt;input element&gt;&#x60; but also &#x60;some plain text&#x60;.
data UiNode = UiNode
  { -- |
    attributes :: Value,
    -- |
    group :: Text,
    -- |
    messages :: [UiText],
    -- |
    meta :: Meta,
    -- |
    _type :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNode where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON UiNode where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- |
data UiNodeAnchorAttributes = UiNodeAnchorAttributes
  { -- | The link's href (destination) URL.  format: uri
    href :: Text,
    -- |
    title :: UiText
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeAnchorAttributes

instance ToJSON UiNodeAnchorAttributes where
  toEncoding = genericToEncoding defaultOptions

-- |
data UiNodeImageAttributes = UiNodeImageAttributes
  { -- | The image's source URL.  format: uri
    src :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeImageAttributes

instance ToJSON UiNodeImageAttributes where
  toEncoding = genericToEncoding defaultOptions

-- | InputAttributes represents the attributes of an input node
data UiNodeInputAttributes = UiNodeInputAttributes
  { -- | Sets the input's disabled field to true or false.
    disabled :: Bool,
    -- |
    label :: Maybe UiText,
    -- | The input's element name.
    name :: Text,
    -- | The input's pattern.
    _pattern :: Maybe Text,
    -- | Mark this input field as required.
    required :: Maybe Bool,
    -- |
    _type :: Text,
    -- | The input's value.
    value :: Maybe Value
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeInputAttributes where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON UiNodeInputAttributes where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- |
data UiNodeTextAttributes = UiNodeTextAttributes
  { -- |
    text :: UiText
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiNodeTextAttributes

instance ToJSON UiNodeTextAttributes where
  toEncoding = genericToEncoding defaultOptions

-- |
data UiText = UiText
  { -- | The message's context. Useful when customizing messages.
    context :: Maybe Value,
    -- |
    id :: Integer,
    -- | The message text. Written in american english.
    text :: Text,
    -- |
    _type :: Text
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON UiText where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

instance ToJSON UiText where
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = typeFieldRename,
          fieldLabelModifier = typeFieldRename
        }

-- | This might include a label and other information that can optionally be used to render UIs.
data Meta = Meta
  { -- |
    label :: Maybe UiText
  }
  deriving stock (Show, Eq, Generic, Data)

instance FromJSON Meta

instance ToJSON Meta where
  toEncoding = genericToEncoding defaultOptions

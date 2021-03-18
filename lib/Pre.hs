module Pre
  ( typeFieldRename,
    FromJSON (..),
    ToJSON (..),
    Value,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
    Options (..),
    defaultOptions,
    Data,
    (&),
    stripPrefix,
    fromMaybe,
    Set,
    ToSchema,
    declareNamedSchema,
    Text,
    module Data.Time,
    UUID,
    Generic,
    Map,
    module Prelude,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import Data.Data (Data)
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Swagger (ToSchema, declareNamedSchema)
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Prelude

typeFieldRename :: String -> String
typeFieldRename "_type" = "type"
typeFieldRename x = x
module OryKratos.Types.Helper (uncapitalize, removeFieldLabelPrefix, customOptions) where

import Data.Aeson.Types (Options (..), defaultOptions)
import qualified Data.Char as Char
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time ()

uncapitalize :: String -> String
uncapitalize (first : rest) = Char.toLower first : rest
uncapitalize [] = []

typeFieldRename :: String -> String
typeFieldRename "_type" = "type"
typeFieldRename "_data" = "data"
typeFieldRename "_pattern" = "pattern"
typeFieldRename x = x

customOptions :: Options
customOptions =
  defaultOptions
    { constructorTagModifier = typeFieldRename,
      fieldLabelModifier = typeFieldRename
    }

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do vice versa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("$", "'Dollar"),
        ("^", "'Caret"),
        ("|", "'Pipe"),
        ("=", "'Equal"),
        ("*", "'Star"),
        ("-", "'Dash"),
        ("&", "'Ampersand"),
        ("%", "'Percent"),
        ("#", "'Hash"),
        ("@", "'At"),
        ("!", "'Exclamation"),
        ("+", "'Plus"),
        (":", "'Colon"),
        (";", "'Semicolon"),
        (">", "'GreaterThan"),
        ("<", "'LessThan"),
        (".", "'Period"),
        ("_", "'Underscore"),
        ("?", "'Question_Mark"),
        (",", "'Comma"),
        ("'", "'Quote"),
        ("/", "'Slash"),
        ("(", "'Left_Parenthesis"),
        (")", "'Right_Parenthesis"),
        ("{", "'Left_Curly_Bracket"),
        ("}", "'Right_Curly_Bracket"),
        ("[", "'Left_Square_Bracket"),
        ("]", "'Right_Square_Bracket"),
        ("~", "'Tilde"),
        ("`", "'Backtick"),
        ("<=", "'Less_Than_Or_Equal_To"),
        (">=", "'Greater_Than_Or_Equal_To"),
        ("!=", "'Not_Equal"),
        ("~=", "'Tilde_Equal"),
        ("\\", "'Back_Slash"),
        ("\"", "'Double_Quote")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace

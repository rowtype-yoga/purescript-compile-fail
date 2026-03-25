module CompileFail.CustomError where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Pattern (Pattern(..), Replacement(..))

extractCustomError :: String -> Maybe String
extractCustomError output = do
  idx <- String.indexOf (Pattern "Custom error:") output
  let raw = String.drop (idx + String.length "Custom error:") output
  let trimmed = trimAtBoundary raw
  pure (String.trim trimmed)
  where
  trimAtBoundary s = boundaries # foldBoundaries s

  boundaries =
    [ "while solving"
    , "while applying"
    , "while inferring"
    , "in value declaration"
    ]

  foldBoundaries :: String -> Array String -> String
  foldBoundaries s [] = s
  foldBoundaries s bs = do
    let go acc b = case String.indexOf (Pattern b) acc of
          Nothing -> acc
          Just i -> String.take i acc
    foldl go s bs

preferredFailureOutput :: String -> String
preferredFailureOutput output = case extractCustomError output of
  Just customError -> customError
  Nothing -> output

escapeForJson :: String -> String
escapeForJson = replaceAll "\\" "\\\\"
  >>> replaceAll "\"" "\\\""
  >>> replaceAll "\n" "\\n"
  >>> replaceAll "\r" "\\r"
  >>> replaceAll "\t" "\\t"
  >>> replaceAll "`" "\\`"
  >>> replaceAll "$" "\\$"
  where
  replaceAll from to = String.replaceAll (Pattern from) (Replacement to)

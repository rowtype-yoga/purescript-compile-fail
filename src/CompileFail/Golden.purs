module CompileFail.Golden where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms (permsAll)
import Node.Path as Path
import Node.Process (lookupEnv)

data GoldenResult
  = GoldenMatch
  | GoldenMismatch { expected :: String, actual :: String, goldenPath :: String }
  | GoldenNew { actual :: String, goldenPath :: String }

checkGolden
  :: { goldenDir :: String, testName :: String, actual :: String }
  -> Aff GoldenResult
checkGolden { goldenDir, testName, actual } = do
  FS.mkdir' goldenDir { recursive: true, mode: permsAll }
  let goldenPath = Path.concat [ goldenDir, testName <> ".golden" ]
  existingResult <- try (FS.readTextFile UTF8 goldenPath)
  case existingResult of
    Left _ -> handleNew goldenPath
    Right content -> handleExisting goldenPath content
  where
  handleNew goldenPath = do
    shouldUpdate <- liftEffect (lookupEnv "UPDATE_GOLDEN")
    case shouldUpdate of
      Just _ -> do
        FS.writeTextFile UTF8 goldenPath actual
        pure GoldenMatch
      Nothing ->
        pure (GoldenNew { actual, goldenPath })

  handleExisting goldenPath content
    | String.trim content == String.trim actual = pure GoldenMatch
    | otherwise = do
        shouldUpdate <- liftEffect (lookupEnv "UPDATE_GOLDEN")
        case shouldUpdate of
          Just _ -> do
            FS.writeTextFile UTF8 goldenPath actual
            pure GoldenMatch
          Nothing ->
            pure (GoldenMismatch { expected: String.trim content, actual: String.trim actual, goldenPath })

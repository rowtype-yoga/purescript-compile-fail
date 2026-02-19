module CompileFail.Spec where

import Prelude

import CompileFail (compileSpagoFile)
import CompileFail.CustomError (extractCustomError)
import CompileFail.Golden (GoldenResult(..), checkGolden)
import CompileFail.HtmlDiff (writeDiffReport)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Aff)
import Node.FS.Aff as FS
import Node.Path as Path
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

type GoldenConfig =
  { projectRoot :: String
  , testDir :: String
  , goldenDir :: String
  , diffDir :: String
  }

defaultConfig :: GoldenConfig
defaultConfig =
  { projectRoot: "."
  , testDir: "compile-fail-tests"
  , goldenDir: "compile-fail-tests/golden"
  , diffDir: "compile-fail-tests/diffs"
  }

goldenTest :: GoldenConfig -> String -> Spec Unit
goldenTest config filePath = do
  let testName = Path.basenameWithoutExt filePath ".purs"
  it testName do
    result <- compileSpagoFile { projectRoot: config.projectRoot } filePath
    let fullOutput = result.stdout <> "\n" <> result.stderr
    let actual = case extractCustomError fullOutput of
          Just err -> err
          Nothing
            | result.exitCode == 0 -> "COMPILATION SUCCEEDED (expected failure)"
            | otherwise -> fullOutput
    goldenResult <- checkGolden { goldenDir: config.goldenDir, testName, actual }
    case goldenResult of
      GoldenMatch -> pure unit
      GoldenNew { goldenPath } ->
        fail ("No golden file at " <> goldenPath <> ", run with UPDATE_GOLDEN=1 to create it")
      GoldenMismatch { expected, actual: act, goldenPath } -> do
        diffPath <- writeDiffReport { outputDir: config.diffDir, testName, expected, actual: act }
        fail ("Golden mismatch for " <> goldenPath <> "\nDiff report: " <> diffPath)

goldenTests :: GoldenConfig -> Aff (Spec Unit)
goldenTests config = do
  files <- FS.readdir config.testDir
  let pursFiles = files
        # Array.filter (String.contains (Pattern ".purs"))
        # map (\f -> Path.concat [ config.testDir, f ])
  pure do
    describe "compile-fail golden tests" do
      for_ pursFiles \filePath ->
        goldenTest config filePath

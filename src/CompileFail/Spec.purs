module CompileFail.Spec where

import Prelude

import CompileFail (compileSpagoFile)
import CompileFail.CustomError (preferredFailureOutput)
import CompileFail.Golden (checkGoldenWithDiff)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
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
    let actual = if result.exitCode == 0
          then "COMPILATION SUCCEEDED (expected failure)"
          else preferredFailureOutput fullOutput
    checkGoldenWithDiff
      { goldenDir: config.goldenDir
      , diffDir: config.diffDir
      , testName
      , actual
      }
      >>= case _ of
        Right _ -> pure unit
        Left message -> fail message

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

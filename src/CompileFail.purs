module CompileFail where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Aff)
import Node.ChildProcess.Types (Exit(..))
import Node.Library.Execa (execa)

type CompileFailResult =
  { file :: String
  , expected :: Maybe String
  , output :: String
  , compilationFailed :: Boolean
  , containsExpected :: Boolean
  }

type CompileContext =
  { sources :: Array String
  , outputDir :: String
  }

-- | Get source globs from spago, excluding test sources
spagoSources :: Aff (Array String)
spagoSources = do
  spawned <- execa "npx" ["spago", "sources"] identity
  result <- spawned.getResult
  pure
    $ Array.filter (\l -> not (String.null l) && not (String.contains (Pattern "test/") l))
    $ String.split (Pattern "\n") result.stdout

-- | Warm the purs compile cache by compiling all library sources
warmCache :: CompileContext -> Aff Unit
warmCache ctx = do
  spawned <- execa "npx" (["purs", "compile", "--output", ctx.outputDir] <> ctx.sources) identity
  void spawned.getResult

-- | Parse the expected error from the first line of a file.
-- | Files should start with: -- EXPECT: <substring>
parseExpect :: String -> Maybe String
parseExpect content = do
  let firstLine = String.take (String.indexOf (Pattern "\n") content # fromMaybe (String.length content)) content
  String.stripPrefix (Pattern "-- EXPECT: ") firstLine

-- | Compile a single file and check whether it fails with the expected error
compileFile :: CompileContext -> String -> Aff CompileFailResult
compileFile ctx filePath = do
  spawned <- execa "npx" (["purs", "compile", "--output", ctx.outputDir] <> ctx.sources <> [filePath]) identity
  result <- spawned.getResult
  let output = result.stdout <> result.stderr
  let compilationFailed = case result.exit of
        Normally 0 -> false
        _ -> true
  let expected = parseExpect output
  pure
    { file: filePath
    , expected
    , output
    , compilationFailed
    , containsExpected: case expected of
        Just substr -> String.contains (Pattern substr) output
        Nothing -> true
    }

-- | Compile a file whose content is provided, checking against the EXPECT comment
compileFileWithContent :: CompileContext -> String -> String -> Aff CompileFailResult
compileFileWithContent ctx filePath content = do
  let expected = parseExpect content
  spawned <- execa "npx" (["purs", "compile", "--output", ctx.outputDir] <> ctx.sources <> [filePath]) identity
  result <- spawned.getResult
  let output = result.stdout <> result.stderr
  let compilationFailed = case result.exit of
        Normally 0 -> false
        _ -> true
  pure
    { file: filePath
    , expected
    , output
    , compilationFailed
    , containsExpected: case expected of
        Just substr -> String.contains (Pattern substr) output
        Nothing -> true
    }

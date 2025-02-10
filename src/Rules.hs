{-# LANGUAGE TypeFamilies #-}

module Rules where

import BnfParser
import Check
import Control.DeepSeq (NFData)
import Data.Data (Typeable)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text.Mixed.Rope as Rope
import Development.IDE.Graph
import GHC.Generics (Generic)
import Diyde.Annotation
import Diyde.SrcSpan (SrcRange)
import LSP.Core.RuleTypes
import LSP.Core.Shake hiding (Log, shakeRecorder)
import qualified LSP.Core.Shake as Shake
import LSP.Core.Types.Diagnostics
import qualified LSP.Core.Types.Diagnostics as LSP
import LSP.Diyde.Rules (semanticTokensUsing)
import LSP.Diyde.Spans
import LSP.Logger
import LSP.Diyde.SemanticTokens
import Language.LSP.Protocol.Types
import SemanticTokens
import Syntax

type instance RuleResult ParseBnf = BnfRules Name
data ParseBnf = ParseBnf
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance RuleResult CheckBnf = BnfRules Resolved
data CheckBnf = CheckBnf
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

data Log
  = LogShake Shake.Log

instance Pretty Log where
  pretty = \case
    LogShake msg -> pretty msg

bnfRules :: Recorder (WithPriority Log) -> Rules ()
bnfRules recorder = do
  define shakeRecorder $ \ParseBnf f -> do
    (_, mRope) <- use_ GetFileContents f
    case mRope of
      Nothing -> pure ([], Nothing)
      Just rope -> do
        let
          contents = Rope.toText rope
        case parseBnf (fromNormalizedFilePath f) contents of
          Left err -> do
            pure (fmap (mkParserFileDiagnostic f) err, Nothing)
          Right bnf ->
            pure ([], Just bnf)

  define shakeRecorder $ \CheckBnf f -> do
    rs <- use_ ParseBnf f
    case check rs of
      Left errs ->
        pure (fmap (mkCheckFileDiagnostic f) errs, Nothing)
      Right res -> do
        pure ([], Just res)

  define shakeRecorder $ \ParserSemanticTokens f -> do
    prog <- use_ ParseBnf f
    case runSemanticTokensM defaultSemanticTokenCtx prog of
      Left _err ->
        pure ([], {- TODO: Log error -} Nothing)
      Right tokenized -> do
        pure ([], Just tokenized)

  define shakeRecorder $ \CheckSemanticTokens f -> do
    prog <- use_ CheckBnf f
    case runSemanticTokensM defaultSemanticTokenCtx prog of
      Left _err ->
        pure ([], {- TODO: Log error -} Nothing)
      Right tokenized -> do
        pure ([], Just tokenized)

  define shakeRecorder $ \GetSemanticTokens f -> do
    toks <-
      semanticTokensUsing
        [ useWithStale CheckSemanticTokens
        , useWithStale ParserSemanticTokens
        ]
        f
    pure ([], Just toks)

  define shakeRecorder $ \GetRelSemanticTokens f -> do
    tokens <- use_ GetSemanticTokens f
    let
      semanticTokens = relativizeTokens $ fmap toSemanticTokenAbsolute tokens
    case encodeTokens defaultSemanticTokensLegend semanticTokens of
      Left _err ->
        pure ([], {- TODO: Log error -} Nothing)
      Right relSemTokens ->
        pure ([], Just relSemTokens)
 where
  shakeRecorder = cmapWithPrio LogShake recorder

  mkParserFileDiagnostic nfp parserError =
    FileDiagnostic
      { fdFilePath = nfp
      , fdShouldShowDiagnostic = ShowDiag
      , fdLspDiagnostic = parserErrorDiag parserError
      , fdOriginalSource = NoMessage
      }

  mkCheckFileDiagnostic nfp checkError =
    FileDiagnostic
      { fdFilePath = nfp
      , fdShouldShowDiagnostic = ShowDiag
      , fdLspDiagnostic = checkErrorDiag checkError
      , fdOriginalSource = NoMessage
      }

  parserErrorDiag :: (Text, SrcRange) -> Diagnostic
  parserErrorDiag (msg, range) =
    Diagnostic
      { _range = srcRangeToLspRange (Just range)
      , _severity = Just LSP.DiagnosticSeverity_Error
      , _code = Nothing
      , _codeDescription = Nothing
      , _source = Just "parser"
      , _message = msg
      , _tags = Nothing
      , _relatedInformation = Nothing
      , _data_ = Nothing
      }

  checkErrorDiag :: CheckError -> Diagnostic
  checkErrorDiag = \case
    UnknownName n ->
      Diagnostic
        { _range = srcRangeToLspRange (rangeOf n)
        , _severity = Just LSP.DiagnosticSeverity_Error
        , _code = Nothing
        , _codeDescription = Nothing
        , _source = Just "check"
        , _message = "No rule with the name " <> nameText n <> " can be found"
        , _tags = Nothing
        , _relatedInformation = Nothing
        , _data_ = Nothing
        }

type instance RuleResult ParserSemanticTokens = [SemanticToken]
data ParserSemanticTokens = ParserSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult CheckSemanticTokens = [SemanticToken]
data CheckSemanticTokens = CheckSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult GetSemanticTokens = [SemanticToken]
data GetSemanticTokens = GetSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

type instance RuleResult GetRelSemanticTokens = [UInt]
data GetRelSemanticTokens = GetRelSemanticTokens
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFData, Typeable, Hashable)

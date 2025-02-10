{-# OPTIONS_GHC -Wno-orphans #-}

module SemanticTokens where

import Data.List.NonEmpty (NonEmpty)
import Diyde.SrcSpan (SrcPos (..), SrcRange (..))
import LSP.Diyde.SemanticTokens
import LSP.Diyde.Spans
import Language.LSP.Protocol.Types
import Syntax

defaultSemanticTokenCtx :: SemanticTokenCtx Token
defaultSemanticTokenCtx =
  SemanticTokenCtx
    { semanticTokenType = \t -> standardTokenType t.category
    , semanticTokenModifier = \_ -> pure []
    }

definesName :: Category -> Maybe [SemanticTokenModifiers]
definesName = \case
  CIdentifier -> Just [SemanticTokenModifiers_Definition]
  _ -> Nothing

withKind :: Kind -> Category -> Maybe SemanticTokenTypes
withKind k = \case
  CIdentifier -> case k of
    Terminal -> Just SemanticTokenTypes_Macro
    NonTerminal -> Just SemanticTokenTypes_Variable
  _ -> Nothing

standardTokenType :: Category -> Maybe SemanticTokenTypes
standardTokenType = \case
  CIdentifier -> Just SemanticTokenTypes_Variable
  CSymbol -> Just SemanticTokenTypes_Operator
  CComment -> Just SemanticTokenTypes_Comment
  CString -> Just SemanticTokenTypes_String
  CWhitespace -> Nothing

deriving anyclass instance (ToSemTokens Token n) => ToSemTokens Token (BnfRules n)
deriving anyclass instance (ToSemTokens Token n) => ToSemTokens Token (Rule n)
deriving anyclass instance (ToSemTokens Token n) => ToSemTokens Token (RuleElement n)
deriving anyclass instance (ToSemTokens Token n) => ToSemTokens Token (NameOrLiteral n)
instance ToSemTokens Token StringLit where
  toSemTokens = \case
    (MkStringLit ann _) -> flattenSemanticTokens ann []

instance (ToSemTokens Token n) => ToSemTokens Token (RuleBody n) where
  toSemTokens = \case
    MkRuleBody ann rs -> flattenSemanticTokens ann (fmap toSemTokens rs)
instance (ToSemTokens Token n) => ToSemTokens Token (RuleHead n) where
  toSemTokens = withModifier (definesName . (.category)) . genericToSemTokens

instance ToSemTokens Token Name where
  toSemTokens = \case
    MkName ann _ -> flattenSemanticTokens ann []

instance ToSemToken Token where
  toSemToken :: Token -> SemanticTokenTypes -> [SemanticTokenModifiers] -> NonEmpty SemanticToken
  toSemToken t tys mods
    | t.range.start.line /= t.range.end.line =
        splitTokens semanticToken t.payload
    | otherwise =
        pure semanticToken
   where
    semanticToken :: SemanticToken
    semanticToken =
      SemanticToken
        { start = srcPosToLspPosition t.range.start
        , length = fromIntegral t.range.length
        , category = tys
        , modifiers = mods
        }

instance ToSemTokens Token Resolved where
  toSemTokens = \case
    MkResolved kind _ actual -> withTokenType (\t -> withKind kind t.category) $ toSemTokens actual

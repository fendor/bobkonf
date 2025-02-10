module LSP.Diyde.Rules where

import Data.Maybe
import qualified Data.Maybe as Maybe
import Development.IDE.Graph
import LSP.Core.PositionMapping
import LSP.Diyde.SemanticTokens
import Language.LSP.Protocol.Types

applyPositionMapping :: [SemanticToken] -> PositionMapping -> [SemanticToken]
applyPositionMapping semTokens positionMapping =
  Maybe.mapMaybe
    ( \t ->
        case toCurrentPosition positionMapping t.start of
          Nothing -> Nothing
          Just newPos -> Just (t{start = newPos})
    )
    semTokens

semanticTokensUsing ::
  [NormalizedFilePath -> Action (Maybe ([SemanticToken], PositionMapping))] ->
  (NormalizedFilePath -> Action [SemanticToken])
semanticTokensUsing phases file = do
  semTokens <- traverse ($ file) phases
  let
    pmSemTokens = fmap (uncurry applyPositionMapping) $ catMaybes semTokens

  case pmSemTokens of
    [] -> pure []
    x : xs -> pure $ foldl mergeSameLengthTokens x xs
 where
  -- We assume that semantic tokens do *not* change its length, no matter whether they
  -- have been lexed, parsed or typechecked.
  -- A rather bold assumption, tbh. It will almost definitely not hold
  -- up in practice, but let's do one step at a time.
  mergeSameLengthTokens :: [SemanticToken] -> [SemanticToken] -> [SemanticToken]
  mergeSameLengthTokens [] bs = bs
  mergeSameLengthTokens as [] = as
  mergeSameLengthTokens (a : as) (b : bs) = case compare a.start b.start of
    -- a.start == b.start
    -- Same token, only print one
    EQ -> a : mergeSameLengthTokens as bs
    -- a.start < b.start
    LT -> a : mergeSameLengthTokens as (b : bs)
    -- a.start > b.start
    GT -> b : mergeSameLengthTokens (a : as) bs

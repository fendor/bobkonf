module Check where

import Control.Monad.Extra (mapMaybeM)
import Control.Monad.State.Class
import Control.Monad.State.Strict (runState)
import Data.Functor.Compose (Compose (..))
import Data.Graph
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import GHC.Generics
import qualified Generics.SOP as SOP
import Optics
import Syntax

data CheckError
  = UnknownName Name
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

type NameMap = Map RawName Resolved

data CheckState = CheckState
  { names :: NameMap
  , errors :: [CheckError]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

check :: BnfRules Name -> Either [CheckError] (BnfRules Resolved)
check rules =
  let
    (checked, s) = runState (checkRules rules) (CheckState Map.empty [])
  in
    case s.errors of
      [] -> Right checked
      errs -> Left errs

topSortRules :: [Rule Name] -> [SCC (Rule Name)]
topSortRules rs = comps
 where
  comps =
    stronglyConnComp
      [ (r, rawName n, fmap rawName $ toListOf gplate body)
      | r@(MkRule _ (MkRuleHead _ n) body) <- rs
      ]

checkRules :: (MonadState CheckState m) => BnfRules Name -> m (BnfRules Resolved)
checkRules = \case
  MkBnfRules ann rules -> do
    checkedRules <- mapMaybeM checkRuleGroup (topSortRules rules)
    pure $ MkBnfRules ann $ List.sort $ concat checkedRules

checkRuleGroup :: (MonadState CheckState m) => SCC (Rule Name) -> m (Maybe [Rule Resolved])
checkRuleGroup = \case
  AcyclicSCC rule -> case rule of
    MkRule ann ruleHead ruleBody ->
      fmap (fmap pure) $
        getCompose $
          MkRule ann
            <$> Compose (checkHead False ruleHead)
            <*> Compose (checkBody ruleBody)
  CyclicSCC rules -> do
    r <-
      traverse
        ( \case
            MkRule ann ruleHead ruleBody ->
              getCompose $
                MkRule ann
                  <$> Compose (checkHead True ruleHead)
                  <*> Compose (checkBody ruleBody)
        )
        rules
    pure $ sequenceA r

checkHead :: (MonadState CheckState m) => Bool -> RuleHead Name -> m (Maybe (RuleHead Resolved))
checkHead recursive = \case
  MkRuleHead ann n -> do
    let
      resolved =
        MkResolved
          { kind = if recursive then NonTerminal else Terminal
          , ref = n
          , actual = n
          }
    modifying' #names (Map.insert (rawName n) resolved)
    pure $
      Just $
        MkRuleHead ann resolved

checkBody :: (MonadState CheckState m) => RuleBody Name -> m (Maybe (RuleBody Resolved))
checkBody = \case
  MkRuleBody ann body ->
    getCompose $
      MkRuleBody ann
        <$> traverse (Compose . checkRuleElement) body

checkRuleElement :: forall m. (MonadState CheckState m) => RuleElement Name -> m (Maybe (RuleElement Resolved))
checkRuleElement = \case
  MkRange ann start end -> pure $ Just $ MkRange ann start end
  MkFragment ann ns ->
    getCompose $
      MkFragment ann
        <$> traverse (Compose . resolveNameOrStringLit) ns
 where
  resolveNameOrStringLit :: NameOrLiteral Name -> m (Maybe (NameOrLiteral Resolved))
  resolveNameOrStringLit = \case
    MkLiteral ann lit -> pure $ Just $ MkLiteral ann lit
    MkNameRef ann n -> fmap (MkNameRef ann) <$> resolveName n

  resolveName :: Name -> m (Maybe Resolved)
  resolveName n = do
    nameMap <- use #names
    case Map.lookup (rawName n) nameMap of
      Nothing -> do
        modifying' #errors (UnknownName n :)
        pure Nothing
      Just res -> pure $ Just (res & #actual .~ n)

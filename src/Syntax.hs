{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax where

import Control.DeepSeq
import Data.Text (Text)
import Diyde.Annotation
import Diyde.SrcSpan (SrcRange (..))
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Optics

type Anno = Anno_ Token ()

data Category
  = CString
  | CIdentifier
  | CComment
  | CWhitespace
  | CSymbol
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data Kind
  = Terminal
  | NonTerminal
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data Resolved = MkResolved
  { kind :: Kind
  , ref :: Name
  , actual :: Name
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data Token = MkToken
  { range :: SrcRange
  , payload :: Text
  , category :: Category
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data RawName = MkRawName Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data Name = MkName Anno RawName
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

nameText :: Name -> Text
nameText = rawNameText . rawName

rawNameText :: RawName -> Text
rawNameText (MkRawName t) = t

rawName :: Name -> RawName
rawName (MkName _ raw) = raw

data BnfRules n = MkBnfRules Anno [Rule n]
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data Rule n = MkRule Anno (RuleHead n) (RuleBody n)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

nameOfHead :: RuleHead n -> n
nameOfHead (MkRuleHead _ n) = n

data RuleHead n = MkRuleHead Anno n
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data RuleElement n
  = MkFragment Anno [NameOrLiteral n]
  | MkRange Anno StringLit StringLit
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data NameOrLiteral n
  = MkNameRef Anno n
  | MkLiteral Anno StringLit
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data StringLit = MkStringLit Anno Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

data RuleBody n = MkRuleBody Anno [RuleElement n]
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, NFData)

newtype BnfSyntax a = MkBnfSyntax a

instance (Generic a, GPosition 1 a a Anno Anno) => HasAnno (BnfSyntax a) where
  type AnnoToken (BnfSyntax a) = Token
  type AnnoExtra (BnfSyntax a) = ()

  setAnno ann (MkBnfSyntax a) = MkBnfSyntax (genericSetAnno ann a)
  getAnno (MkBnfSyntax a) = genericGetAnno a

deriving via
  BnfSyntax (RuleHead n)
  instance
    HasAnno (RuleHead n)
deriving via
  BnfSyntax (RuleBody n)
  instance
    HasAnno (RuleBody n)
deriving via
  BnfSyntax (RuleElement n)
  instance
    HasAnno (RuleElement n)
deriving via
  BnfSyntax StringLit
  instance
    HasAnno StringLit
deriving via
  BnfSyntax (NameOrLiteral n)
  instance
    HasAnno (NameOrLiteral n)
deriving via
  BnfSyntax (Rule n)
  instance
    HasAnno (Rule n)
deriving via
  BnfSyntax (BnfRules n)
  instance
    HasAnno (BnfRules n)
deriving via
  BnfSyntax Name
  instance
    HasAnno Name

deriving anyclass instance HasSrcRange (RuleHead n)
deriving anyclass instance HasSrcRange (RuleBody n)
deriving anyclass instance HasSrcRange (RuleElement n)
deriving anyclass instance HasSrcRange (Rule n)
deriving anyclass instance HasSrcRange (BnfRules n)
deriving anyclass instance HasSrcRange Name
deriving anyclass instance HasSrcRange (NameOrLiteral n)
deriving anyclass instance HasSrcRange StringLit

deriving anyclass instance (ToConcreteNodes Token n) => ToConcreteNodes Token (RuleHead n)
deriving anyclass instance (ToConcreteNodes Token n) => ToConcreteNodes Token (RuleBody n)
deriving anyclass instance (ToConcreteNodes Token n) => ToConcreteNodes Token (Rule n)
deriving anyclass instance (ToConcreteNodes Token n) => ToConcreteNodes Token (BnfRules n)
deriving anyclass instance (ToConcreteNodes Token n) => ToConcreteNodes Token (RuleElement n)
deriving anyclass instance (ToConcreteNodes Token n) => ToConcreteNodes Token (NameOrLiteral n)

instance ToConcreteNodes Token StringLit where
  toNodes (MkStringLit ann _) = flattenConcreteNodes ann []

instance ToConcreteNodes Token Name where
  toNodes (MkName ann _) = flattenConcreteNodes ann []

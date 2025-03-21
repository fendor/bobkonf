module Diyde.SrcSpan where

import Base

import Control.DeepSeq
import qualified Generics.SOP as SOP
import qualified Data.Text as Text
import Data.TreeDiff.Class (ToExpr)

-- ----------------------------------------------------------------------------
-- SrcSpan
-- ----------------------------------------------------------------------------

data SrcSpan = MkSrcSpan
  { start :: !SrcPos
  , end :: !SrcPos
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic)

instance Semigroup SrcSpan where
  -- Always expand the 'SrcSpan' s.t. it now covers both 'SrcSpan's.
  s1 <> s2 = MkSrcSpan (min s1.start s2.start) (max s1.end s2.end)

fromSrcRange :: SrcRange -> SrcSpan
fromSrcRange rng = MkSrcSpan rng.start rng.end

posInRange :: SrcPos -> SrcSpan -> Bool
posInRange p (MkSrcSpan start end) =
  start <= p && p <= end

spansOverlap :: SrcSpan -> SrcSpan -> Bool
spansOverlap s1 s2 =
  s1.start <= s2.end && s2.start <= s1.end

subRangeOf :: SrcSpan -> SrcSpan -> Bool
subRangeOf s1 s2 =
  s2.start <= s1.start && s1.end <= s2.end

rangeBefore :: SrcSpan -> SrcSpan -> Bool
rangeBefore s1 s2 =
  s1.end < s2.start

rangeAfter :: SrcSpan -> SrcSpan -> Bool
rangeAfter s1 s2 =
  s1.start < s2.end

prettySrcSpan :: SrcSpan -> Text
prettySrcSpan (MkSrcSpan p1 p2) = prettySrcPos p1 <> prettyPartialSrcPos p1 p2

prettySrcPos :: SrcPos -> Text
prettySrcPos (MkSrcPos l c) = Text.pack (show l) <> ":" <> Text.pack (show c)

prettyPartialSrcPos :: SrcPos -> SrcPos -> Text
prettyPartialSrcPos (MkSrcPos rl rc) p@(MkSrcPos l c)
  | rl == l && rc == c = ""
  | rl == l = "-" <> Text.pack (show c)
  | otherwise = "-" <> prettySrcPos p


-- | A range of source positions. We store the length of a range as well.
data SrcRange =
  MkSrcRange
    { start   :: !SrcPos -- inclusive
    , end     :: !SrcPos -- inclusive
    , length  :: !Int
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr, NFData)

-- | A single source position. Line and column numbers are 1-based.
data SrcPos =
  MkSrcPos
    {
    -- filename :: !FilePath
      line     :: !Int
    , column   :: !Int
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr, NFData)

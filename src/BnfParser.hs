{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module BnfParser where

import Control.Arrow (second)
import qualified Control.Monad.State.Strict as State
import Data.Char
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Diyde.Annotation
import Diyde.Parser.Annotation
import qualified Diyde.ParserCombinators as Diyde
import Diyde.SrcSpan (SrcPos (..), SrcRange (..))
import Optics
import Syntax
import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as Parser

type Parser = Parsec Void Text

type Lexeme = Lexeme_ Token
type Epa = Epa_ Token

space :: Parser Token
space = do
  start <- getSourcePos
  ws <- takeWhileP (Just "whitespace") isSpace
  end <- getSourcePos
  pure $
    MkToken
      { range = mkSrcRange start end (Text.length ws)
      , payload = ws
      , category = CWhitespace
      }

lexeme :: Parser a -> Parser (Lexeme a)
lexeme p = do
  a <- p
  ws <- space
  pure $ mkLexeme [ws] a

epa :: Parser Token -> Parser (Epa Text)
epa p = do
  l <- lexeme p
  pure $ mkEpaFromLexeme (l.payload.payload) l

textToken :: Category -> Text -> Parser (Epa Text)
textToken cat t = epa $ do
  start <- getSourcePos
  a <- Parser.string t
  end <- getSourcePos
  pure $
    MkToken
      { range = mkSrcRange start end (Text.length a)
      , payload = a
      , category = cat
      }

symbol :: Text -> Parser (Epa Text)
symbol = textToken CSymbol

identifier :: Parser (Epa Text)
identifier = epa $ do
  start <- getSourcePos
  a <- takeWhile1P (Just "identifier") (\c ->
      isAlphaNum c || c `elem` ['<', '-', '>', '_', ':'])
  end <- getSourcePos
  pure $
    MkToken
      { range = mkSrcRange start end (Text.length a)
      , payload = a
      , category = CIdentifier
      }

literal :: Parser (Epa Text)
literal = epa $ do
  start <- getSourcePos
  (s, a', e) <- doubleQuoted <|> singleQuoted
  let
    a = Text.singleton s <> Text.pack a' <> Text.singleton e
  end <- getSourcePos
  pure $
    MkToken
      { range = mkSrcRange start end (Text.length a)
      , payload = a
      , category = CString
      }
 where
  singleQuoted :: Parser (Char, String, Char)
  singleQuoted = Diyde.between (Parser.char '"') (Parser.char '"') (many $ anySingleBut '"')
  doubleQuoted :: Parser (Char, String, Char)
  doubleQuoted = Diyde.between (Parser.char '\'') (Parser.char '\'') (many $ anySingleBut '\'')

stringLit :: Parser StringLit
stringLit = attachAnno $ MkStringLit emptyAnno <$> annoEpa literal

name :: Parser Name
name =
  attachAnno $
    MkName emptyAnno
      <$> annoEpa (fmap MkRawName <$> identifier)

ruleElement :: Parser (RuleElement Name)
ruleElement =
  attachAnno $
    tryParser
      ( MkRange emptyAnno
          <$> annoHole stringLit
          <* annoEpa (symbol "..")
          <*> annoHole stringLit
      )
      <|> MkFragment emptyAnno
      <$> annoHole (many nameOrLit)

nameOrLit :: Parser (NameOrLiteral Name)
nameOrLit =
  attachAnno $
    MkLiteral emptyAnno
      <$> annoHole stringLit
        <|> MkNameRef emptyAnno
      <$> annoHole name

ruleName :: Parser (RuleHead Name)
ruleName =
  attachAnno $
    MkRuleHead emptyAnno <$> annoHole name

rule :: Parser (Rule Name)
rule =
  attachAnno $ do
    name' <- annoHole ruleName
    _ <- annoEpa (symbol "::=")
    body <- annoHole ruleBody
    _ <- annoEpa (symbol ";")
    pure $ MkRule emptyAnno name' body

ruleBody :: Parser (RuleBody Name)
ruleBody = do
  s <- Diyde.sepBy ruleElement (symbol "|")
  pure $
    MkRuleBody
      ( mkAnno $ Diyde.toListSepBy mkHoleWithSrcRange mkEpaAnno s
      )
      (Diyde.sepByElems s)

rules :: Parser (BnfRules Name)
rules = do
  ws <- space
  attachAnno $
    MkBnfRules emptyAnno
      <$ annoLexemes (pure $ mkLexeme [ws] [])
      <*> annoHole (many rule)

-- ----------------------------------------------------------------------------
-- Main entry point
-- ----------------------------------------------------------------------------

parseBnf :: FilePath -> Text -> Either [(Text, SrcRange)] (BnfRules Name)
parseBnf fp input =
  case parse rules fp input of
    Left bundle ->
      Left $ fmap (second toRange) $ Foldable.toList $ errorBundleToErrorMessages bundle
    Right r ->
      Right r
 where
  toRange s = MkSrcRange (sourcePosToSrcPos s) (sourcePosToSrcPos s & #column %~ (+ 5)) 5

-- ----------------------------------------------------------------------------
-- Megaparsec specific helpers
-- ----------------------------------------------------------------------------

sourcePosToSrcPos :: SourcePos -> SrcPos
sourcePosToSrcPos sp =
  MkSrcPos
    { line = unPos $ sourceLine sp
    , column = unPos $ sourceColumn sp
    }

mkSrcRange :: SourcePos -> SourcePos -> Int -> SrcRange
mkSrcRange start end len =
  MkSrcRange
    { start = sourcePosToSrcPos start
    , end = sourcePosToSrcPos end
    , length = len
    }

errorBundleToErrorMessages ::
  forall s e.
  ( VisualStream s
  , TraversableStream s
  , ShowErrorComponent e
  ) =>
  -- | Parse error bundle to display
  ParseErrorBundle s e ->
  -- | Textual rendition of the bundle
  NonEmpty (Text, SourcePos)
errorBundleToErrorMessages ParseErrorBundle{..} =
  let
    (results, _) = State.runState (traverse format bundleErrors) bundlePosState
  in
    results
 where
  format :: ParseError s e -> State.State (PosState s) (Text, SourcePos)
  format e = do
    pst <- State.get
    let
      (msline, pst') = calculateOffset pst
      epos = pstateSourcePos pst'
      errMsg = parseErrorTextPretty e
      parseErrCtx = offendingLine msline epos
    State.put pst'
    pure $ (Text.pack $ parseErrCtx <> errMsg, epos)
   where
    calculateOffset pst = reachOffset (errorOffset e) pst
    offendingLine msline epos =
      case msline of
        Nothing -> ""
        Just sline ->
          let
            rpadding =
              if pointerLen > 0
                then replicate rpshift ' '
                else ""
            pointerLen =
              if rpshift + elen > slineLen
                then slineLen - rpshift + 1
                else elen
            pointer = replicate pointerLen '^'
            lineNumber = (show . unPos . sourceLine) epos
            padding = replicate (length lineNumber + 1) ' '
            rpshift = unPos (sourceColumn epos) - 1
            slineLen = length sline
          in
            padding
              <> "|\n"
              <> lineNumber
              <> " | "
              <> sline
              <> "\n"
              <> padding
              <> "| "
              <> rpadding
              <> pointer
              <> "\n"
    pxy = Proxy :: Proxy s
    elen =
      case e of
        TrivialError _ Nothing _ -> 1
        TrivialError _ (Just x) _ -> errorItemLength pxy x
        FancyError _ xs ->
          Set.foldl' (\a b -> max a (errorFancyLength b)) 1 xs

-- | Get length of the “pointer” to display under a given 'ErrorItem'.
errorItemLength :: (VisualStream s) => Proxy s -> ErrorItem (M.Token s) -> Int
errorItemLength pxy = \case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

-- | Get length of the “pointer” to display under a given 'ErrorFancy'.
errorFancyLength :: (ShowErrorComponent e) => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1

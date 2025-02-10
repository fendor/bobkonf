{-# LANGUAGE EmptyCase #-}

module Handlers where

import Config
import Control.Lens ((^.))
import Control.Monad.IO.Class
import qualified Data.Text as Text
import LSP.Core.Service hiding (Log, LogShake)
import LSP.Core.Shake hiding (Log)
import LSP.Core.Types.Location
import LSP.Diyde.LanguageServer hiding (Log)
import LSP.Logger
import qualified Language.LSP.Protocol.Lens as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server (Handlers)
import Rules hiding (Log, LogShake)

data Log

instance Pretty Log where
  pretty = \case {}

handlers :: Recorder (WithPriority Log) -> Handlers (ServerM Config)
handlers _recorder =
  mconcat
    [ requestHandler SMethod_TextDocumentSemanticTokensFull $ \ide params -> do
        let
          uri = params ^. J.textDocument . J.uri
          nfp = fromUri $ toNormalizedUri uri
        tokens <-
          liftIO $
            runAction "semanticTokens" ide $
              use GetRelSemanticTokens nfp
        case tokens of
          Nothing -> do
            pure $
              Left $
                TResponseError
                  { _code = InL LSPErrorCodes_RequestFailed
                  , _message = "Internal error, failed to produce semantic tokens for \"" <> Text.pack (show uri) <> "\""
                  , _xdata = Nothing
                  }
          Just semanticTokensData -> do
            pure $
              Right $
                InL $
                  SemanticTokens
                    { _resultId = Nothing
                    , _data_ = semanticTokensData
                    }
    ]

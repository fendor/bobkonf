module Server where

import Config
import Data.Functor (void)
import Data.Text (Text)
import qualified Handlers
import LSP.Core.Shake hiding (Log)
import LSP.Diyde.LanguageServer hiding (Log)
import qualified LSP.Diyde.LanguageServer as LanguageServer
import LSP.Diyde.Server hiding (Log)
import qualified LSP.Diyde.Server as LsServer
import LSP.Logger
import Rules hiding (Log)
import qualified Rules

defaultMain :: IO ()
defaultMain = do
  -- Setup the logger
  recorder <- makeDefaultStderrRecorder Nothing
  let
    prettyRecorder = cmapWithPrio pretty recorder
    serverRecorder = cmapWithPrio LogServer prettyRecorder

  -- Get Arguments.
  -- If we wanted to, here is where we would add argument parsing
  args <- getDefaultArguments
  -- Run the Language Server in all its glory!
  serverMain serverRecorder (bnfServerConfig prettyRecorder) args

data Log
  = LogConfigurationChange Text
  | LogRules Rules.Log
  | LogHandlers Handlers.Log
  | LogServer LsServer.Log
  | LogLanguageServer LanguageServer.Log

instance Pretty Log where
  pretty = \case
    LogConfigurationChange msg -> "Configuration changed:" <+> pretty msg
    LogRules msg -> pretty msg
    LogHandlers msg -> pretty msg
    LogServer msg -> pretty msg
    LogLanguageServer msg -> pretty msg

bnfServerConfig :: Recorder (WithPriority Log) -> ServerConfig ServerM Config
bnfServerConfig recorder =
  ServerConfig
    { config = defConfig
    , parseServerConfig = parseServerConfig
    , onConfigChange = defOnConfigChange serverRecorder
    , rules = Rules.bnfRules (cmapWithPrio LogRules recorder)
    , handlers = lspHandlers
    , kick = kickRules $ \files -> do
        void $
          uses ParseBnf files
            *> uses CheckBnf files
    , setupLsp = \fp getIdeState ->
        setupLSP
          (cmapWithPrio LogLanguageServer recorder)
          fp
          lspHandlers
          getIdeState
    , keywords = []
    , configSection = "bnf"
    , lspOptions = LsServer.lspOptions
    }
 where
  lspHandlers = mconcat
    [ defHandlers languageServerRecorder
    , Handlers.handlers (cmapWithPrio LogHandlers recorder)
    ]
  serverRecorder = cmapWithPrio LogServer recorder
  languageServerRecorder = cmapWithPrio LogLanguageServer recorder

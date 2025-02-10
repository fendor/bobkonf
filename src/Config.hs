module Config where

import Data.Aeson
import GHC.Generics (Generic)

data Config = Config
  { serverExecutablePath :: FilePath
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

defConfig :: Config
defConfig = Config "bobkonf-lsp"

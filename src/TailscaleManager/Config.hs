-- | Config loader for Tailscale Routes Manager

module TailscaleManager.Config where

import Data.Aeson
import Data.Aeson.IP ()
import Data.ByteString.Lazy qualified as LB
import Data.IP (IPRange)
import Data.Maybe (fromMaybe)

-- |Parse our config file.  May throw AesonException on failure.
loadConfig :: FilePath -> IO TSConfig
loadConfig fp = LB.readFile fp >>= throwDecode

-- |Config file schema
data TSConfig
  = TSConfig
  { tsRoutes :: [IPRange]
  , tsHostRoutes :: [String]
  , tsExtraArgs :: [String]
  , tsAWSManagedPrefixLists :: [String]
  }
  deriving Show

-- |Config file JSON parser
instance FromJSON TSConfig where
  parseJSON = withObject "TSConfig" $ \obj -> do
    routes <- obj .:? "routes"
    hostRoutes <- obj .:? "hostRoutes"
    extraArgs <- obj .:? "extraArgs"
    awsManagedPrefixLists <- obj .:? "awsManagedPrefixLists"
    return (TSConfig { tsRoutes = fromMaybe [] routes
                     , tsHostRoutes = fromMaybe [] hostRoutes
                     , tsExtraArgs = fromMaybe [] extraArgs
                     , tsAWSManagedPrefixLists = fromMaybe [] awsManagedPrefixLists
                     })

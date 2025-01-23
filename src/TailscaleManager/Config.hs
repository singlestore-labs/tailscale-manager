-- | Config loader for Tailscale Routes Manager

module TailscaleManager.Config where

import Data.Aeson
import Data.Aeson.IP ()
import Data.IP (IPRange)
import Data.Maybe (fromMaybe)
import Data.Text
import System.FilePath (takeExtension)
import Data.Yaml (decodeFileEither)

-- |Config file schema
data TSConfig
  = TSConfig
  { tsRoutes :: [IPRange]
  , tsHostRoutes :: [String]
  , tsExtraArgs :: [String]
  , tsAWSManagedPrefixLists :: [Text]
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

-- | Load configuration from a file, detecting format based on file extension.
loadConfig :: FilePath -> IO TSConfig
loadConfig path = do
  case takeExtension path of
    ".json" -> loadConfigFromJSON path
    ".yaml" -> loadConfigFromYAML path
    _       -> error "Unsupported file format. Please use .json or .yaml."

-- | Load configuration from a JSON file.
loadConfigFromJSON :: FilePath -> IO TSConfig
loadConfigFromJSON path = do
  result <- eitherDecodeFileStrict path
  case result of
    Left err -> error $ "Failed to parse JSON config: " ++ err
    Right config -> return config

-- | Load configuration from a YAML file.
loadConfigFromYAML :: FilePath -> IO TSConfig
loadConfigFromYAML path = do
  result <- decodeFileEither path
  case result of
    Left err -> error $ "Failed to parse YAML config: " ++ show err
    Right config -> return config

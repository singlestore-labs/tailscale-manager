{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Exception (try)
import Data.Aeson
import Data.ByteString.Lazy qualified as LB
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.IP (fromSockAddr, IP)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import GHC.IO.Exception (IOException)
import Network.Socket
import Options.Applicative
import System.Log.Logger

data MyFlags
  = MyFlags
  { configFile :: FilePath
  , dryRun :: Bool
  }

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (myFlags <**> helper)
           (fullDesc
            <> progDesc "Tailscale options manager"
            <> header "tailscale-manager")
    myFlags = MyFlags
              <$> argument str (metavar "<configfile.json>")
              <*> switch (long "dryrun"
                          <> help "Dryrun mode")

run :: MyFlags -> IO ()
run opts = do
  config <- loadConfig (configFile opts)
  tsArgs <- generateTailscaleArgs config
  logger <- getLogger "tailscale-manager"
  if dryRun opts
    then do
      logL logger WARNING "Dry-run mode enabled.  NOT actually executing commands."
      putStrLn (unwords tsArgs)
    else do
      undefined

-- Parse our config file.  May throw AesonException on failure.
loadConfig :: FilePath -> IO TSConfig
loadConfig fp = LB.readFile fp >>= throwDecode

generateTailscaleArgs :: TSConfig -> IO [String]
generateTailscaleArgs config = do
  hostIPs <- resolveHostnames (tsHostRoutes config)
  let mergedRoutes = tsRoutes config ++ map ipToHostRoute hostIPs
  return $ [ "tailscale", "set"
           , "--advertise-routes=" ++ intercalate "," mergedRoutes
           , "--advertise-exit-node=" ++ show (tsAdvertiseExitNode config)
           ] ++ tsExtraArgs config

ipToHostRoute :: IP -> String
ipToHostRoute ip = show ip ++ "/32"

-- Resolve a list of hostnames to a concatenated list of IPs.
resolveHostnames :: [HostName] -> IO [IP]
resolveHostnames hs = mapM resolveOne hs <&> concat . catMaybes

-- Resolve one hostname to a list of IPs.
-- Prints a message to stderr and returns Nothing if the lookup fails.
resolveOne :: HostName -> IO (Maybe [IP])
resolveOne hostname = do
  logger <- getLogger "resolveOne"
  let hints = defaultHints { addrSocketType = Stream }
  result <- try @IOException $ getAddrInfo (Just hints) (Just hostname) Nothing
  case result of
    Left err -> do
      logL logger WARNING (show err)
      return Nothing
    Right addrInfos ->
      return $ Just (map fst (mapMaybe (fromSockAddr . addrAddress) addrInfos))

data TSConfig
  = TSConfig
  { tsRoutes :: [String]
  , tsHostRoutes :: [String]
  , tsAdvertiseExitNode :: Bool
  , tsExtraArgs :: [String]
  }
  deriving Show

instance FromJSON TSConfig where
  parseJSON = withObject "TSConfig" $ \obj -> do
    routes <- obj .:? "routes"
    hostRoutes <- obj .:? "hostRoutes"
    advertiseExitNode <- obj .:? "advertisExitNode"
    extraArgs <- obj .:? "extraArgs"
    return (TSConfig { tsRoutes = fromMaybe [] routes
                     , tsHostRoutes = fromMaybe [] hostRoutes
                     , tsAdvertiseExitNode = fromMaybe False advertiseExitNode
                     , tsExtraArgs = fromMaybe [] extraArgs
                     })

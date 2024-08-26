{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (forever, when)
import Data.Aeson
import Data.ByteString.Lazy qualified as LB
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.IP (fromSockAddr, IP)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Set qualified as Set
import GHC.IO.Exception (IOException)
import Network.Socket
import Options.Applicative
import Prettyprinter (Doc)
import System.Log.Logger
import System.Process (callProcess, showCommandForUser)
import Text.RawString.QQ (r)

type Seconds = Int

data MyFlags
  = MyFlags
  { configFile :: FilePath
  , dryRun :: Bool
  , tailscaleCmd :: FilePath
  , interval :: Seconds
  }

helpText :: Doc a
helpText = [r|Tailscale routes manager

Dynamically resolves a list of hostRoutes to IP addresses,
then tells tailscale to advertise them as /32 routes along with any
normal CIDR routes.

Config file example:

{
  "routes": [
    "172.16.0.0/22",
    "192.168.0.0/24"
  ],
  "hostRoutes": [
    "special-hostname1.example",
    "special-hostname2.example",
  ],
  "extraArgs": ["--webclient"]
}|]

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (myFlags <**> helper)
           (fullDesc
            <> progDescDoc (Just helpText)
            <> header "tailscale-manager")
    myFlags = MyFlags
              <$> argument str (metavar "<configfile.json>")
              <*> switch (long "dryrun"
                          <> help "Dryrun mode")
              <*> strOption (long "tailscale"
                             <> metavar "PATH"
                             <> help "Path to the tailscale executable"
                             <> value "tailscale"
                             <> showDefault)
              <*> option auto (long "interval"
                               <> metavar "INT"
                               <> help "Interval (in seconds) between runs. 0 means exit after running once."
                               <> value 0
                               <> showDefault)

run :: MyFlags -> IO ()
run flags = do
  config <- loadConfig (configFile flags)
  tsArgs <- generateTailscaleArgs config
  logger <- getLogger "tailscale-manager" <&> setLevel INFO
  let escapedArgs = showCommandForUser (tailscaleCmd flags) tsArgs
      runOnce | dryRun flags =
                  logL logger INFO $ "(not actually) Runnning: " ++ escapedArgs
              | otherwise = do
                  logL logger INFO $ "Running: " ++ escapedArgs
                  callProcess (tailscaleCmd flags) tsArgs

  when (dryRun flags) $
    logL logger WARNING "Dry-run mode enabled."

  if interval flags > 0
    then do
      logL logger INFO ("Running every " ++ show (interval flags) ++ " seconds")
      forever $ runOnce >> threadDelay (interval flags * 1000000)  -- microseconds
    else runOnce

-- Parse our config file.  May throw AesonException on failure.
loadConfig :: FilePath -> IO TSConfig
loadConfig fp = LB.readFile fp >>= throwDecode

generateTailscaleArgs :: TSConfig -> IO [String]
generateTailscaleArgs config = do
  hostIPs <- resolveHostnames (tsHostRoutes config)
  -- Converting to Set and back is faster than (nub . sort) on a List
  let mergedRoutes = (Set.toList . Set.fromList) (tsRoutes config ++ map ipToHostRoute hostIPs)
  return $ [ "set", "--advertise-routes=" ++ intercalate "," mergedRoutes
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
  , tsExtraArgs :: [String]
  }
  deriving Show

instance FromJSON TSConfig where
  parseJSON = withObject "TSConfig" $ \obj -> do
    routes <- obj .:? "routes"
    hostRoutes <- obj .:? "hostRoutes"
    extraArgs <- obj .:? "extraArgs"
    return (TSConfig { tsRoutes = fromMaybe [] routes
                     , tsHostRoutes = fromMaybe [] hostRoutes
                     , tsExtraArgs = fromMaybe [] extraArgs
                     })

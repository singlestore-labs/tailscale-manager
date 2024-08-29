{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (when, void)
import Control.Monad.Loops (iterateM_)
import Data.Aeson
import Data.ByteString.Lazy qualified as LB
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.IP (fromSockAddr, IP)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Set qualified as Set
import Network.Socket
import Options.Applicative
import Prettyprinter (Doc)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (LogHandler(setFormatter))
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger
import System.Process (callProcess, showCommandForUser)
import Text.RawString.QQ (r)

type Seconds = Int

data TailscaleManagerOptions
  = TailscaleManagerOptions
  { configFile :: FilePath
  , dryRun :: Bool
  , tailscaleCmd :: FilePath
  , interval :: Seconds
  , maxShrinkRatio :: Float
  }

-- This is just a multiline string.
-- Don't be fooled by gitlab trying to syntax highlight it.
helpText :: Doc a
helpText = [r|Tailscale routes manager

Dynamically resolves a list of hostRoutes to IP addresses, then tells tailscale
to advertise them as /32 routes along with any normal CIDR routes.

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

myLogger :: IO Logger
myLogger = do
  let myFormatter = simpleLogFormatter "[$time $prio $loggername] $msg"
  handler <- streamHandler stderr INFO <&> flip setFormatter myFormatter
  getRootLogger <&> setLevel INFO . setHandlers [handler]

tsManagerOptions :: Parser TailscaleManagerOptions
tsManagerOptions =
  TailscaleManagerOptions
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
  <*> option auto (long "max-shrink-ratio"
                   <> metavar "RATIO"
                   <> help "Max allowed route shrinkage between consecutive runs, as a ratio between 0 and 1. 1 means no limit."
                   <> value 0.33
                   <> showDefault)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (tsManagerOptions <**> helper)
           (fullDesc
            <> progDescDoc (Just helpText)
            <> header "tailscale-manager")

run :: TailscaleManagerOptions -> IO ()
run options = do
  logger <- myLogger
  when (dryRun options) $
    logL logger WARNING "Dry-run mode enabled."
  if interval options > 0
    then iterateM_ (runOnce options) Set.empty
    else void (runOnce options Set.empty)

runOnce :: TailscaleManagerOptions -> Set.Set String -> IO (Set.Set String)
runOnce options prevRoutes = do
  logger <- myLogger

  let invokeTailscale args =
        if dryRun options
          then
            logL logger INFO $ "(not actually) Runnning: " ++ escapedArgs
          else do
            logL logger INFO $ "Running: " ++ escapedArgs
            callProcess (tailscaleCmd options) args
        where escapedArgs = showCommandForUser (tailscaleCmd options) args

  let logDelay = do
        logL logger INFO ("Sleeping for " ++ show (interval options) ++ " seconds")
        threadDelay (interval options * 1000000)  -- microseconds

  config <- loadConfig (configFile options)
  newRoutes <- generateRoutes config
  isSane <- sanityCheck (maxShrinkRatio options) prevRoutes newRoutes
  if isSane
    then do
      invokeTailscale $ ["set", "--advertise-routes=" ++ intercalate "," (Set.toList newRoutes)] ++ tsExtraArgs config
      logDelay
      return newRoutes
    else do
      logL logger ERROR "Sanity check failed! Refusing to apply changes!"
      logDelay
      return prevRoutes

sanityCheck :: Show a => Float -> Set.Set a -> Set.Set a -> IO Bool
sanityCheck shrinkThreshold oldState newState = do
  logger <- myLogger
  let ratio = fromIntegral (length oldState) / fromIntegral (length newState)
  when (ratio > 0) $
    logL logger INFO $ "Shrink ratio: " ++ show ratio ++ " (Old: " ++ show oldState ++ " New: " ++ show newState ++ ")"
  return (ratio < (1 / shrinkThreshold))

-- Parse our config file.  May throw AesonException on failure.
loadConfig :: FilePath -> IO TSConfig
loadConfig fp = LB.readFile fp >>= throwDecode

generateRoutes :: TSConfig -> IO (Set.Set String)
generateRoutes config = do
  hostIPs <- resolveHostnames (tsHostRoutes config)
  return $ Set.fromList (tsRoutes config ++ map ipToHostRoute hostIPs)

ipToHostRoute :: IP -> String
ipToHostRoute ip = show ip ++ "/32"

-- Resolve a list of hostnames to a concatenated list of IPs.
resolveHostnames :: [HostName] -> IO [IP]
resolveHostnames hs = mapM resolveOne hs <&> concat . catMaybes

-- Resolve one hostname to a list of IPs.
-- Logs a warning and returns Nothing if the lookup fails.
resolveOne :: HostName -> IO (Maybe [IP])
resolveOne hostname = do
  logger <- myLogger
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

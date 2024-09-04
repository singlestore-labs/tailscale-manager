{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module TailscaleManager where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Control.Monad.Loops (iterateM_)
import Data.Aeson
import Data.Aeson.IP ()
import Data.ByteString.Lazy qualified as LB
import Data.Functor ((<&>))
import Data.IP (IP (IPv4, IPv6), IPRange (IPv4Range, IPv6Range), fromSockAddr, makeAddrRange)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Network.Socket
import Options.Applicative
import Prettyprinter (Doc)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (LogHandler (setFormatter))
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger
import System.Process (callProcess, showCommandForUser)
import Text.RawString.QQ (r)

type Seconds = Int

-- | Commandline option types
data TailscaleManagerOptions
  = TailscaleManagerOptions
  { configFile :: FilePath
  , dryRun :: Bool
  , tailscaleCmd :: FilePath
  , interval :: Seconds
  , maxShrinkRatio :: Double
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
  getLogger "tailscale-manager" <&> setLevel INFO . setHandlers [handler]

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
  -- Clear the default root log handler to prevent duplicate messages
  updateGlobalLogger rootLoggerName removeHandler
  logger <- myLogger
  when (dryRun options) $
    logL logger WARNING "Dry-run mode enabled. Will not actually apply changes."
  if interval options > 0
    then iterateM_ (runOnce options) S.empty
    else void (runOnce options S.empty)

runOnce :: TailscaleManagerOptions  -- ^ Commandline options
        -> Set IPRange              -- ^ Result of the previous run
        -> IO (Set IPRange)         -- ^ New generated routes
runOnce options prevRoutes = do
  logger <- myLogger

  let invokeTailscale args =
        if dryRun options
          then
            logL logger DEBUG $ "(not actually) Running: " ++ escapedArgs
          else do
            logL logger DEBUG $ "Running: " ++ escapedArgs
            callProcess (tailscaleCmd options) args
        where escapedArgs = showCommandForUser (tailscaleCmd options) args

  let logDelay = do
        when (interval options > 0) $
          logL logger INFO ("Sleeping for " ++ show (interval options) ++ " seconds")
        threadDelay (interval options * 1000000)  -- microseconds

  config <- loadConfig (configFile options)
  newRoutes <- generateRoutes config

  logDiff prevRoutes newRoutes

  let shrinkage = shrinkRatio prevRoutes newRoutes
  if shrinkage < maxShrinkRatio options
    then do
      invokeTailscale $ ["set", "--advertise-routes=" ++ intercalate "," (map show $ S.toList newRoutes)] ++ tsExtraArgs config
      logDelay
      return newRoutes
    else do
      logL logger ERROR "Sanity check failed! Refusing to apply changes!"
      logL logger ERROR ("Shrink ratio: " ++ show shrinkage)
      logDelay
      return prevRoutes

-- |Emit a log message describing the difference between old and new route sets.
logDiff :: Set IPRange -> Set IPRange -> IO ()
logDiff prevRoutes newRoutes = do
  logger <- myLogger

  logL logger INFO (show (length routesToAdd)     ++ " to add, " ++
                    show (length routesToRemove)  ++ " to remove, " ++
                    show (length routesUnchanged) ++ " routes unchanged")
  unless (null routesToAdd) $
    logL logger INFO ("Routes to add: "    ++ unwords (map show (S.toList routesToAdd)))
  unless (null routesToRemove) $
    logL logger INFO ("Routes to remove: " ++ unwords (map show (S.toList routesToRemove)))
  where
    routesToAdd     = S.difference newRoutes prevRoutes
    routesToRemove  = S.difference prevRoutes newRoutes
    routesUnchanged = S.intersection prevRoutes newRoutes

-- |Compute how much the smaller the new set is vs old.
--
-- >>> shrinkRatio ["1.1.1.1/32", "2.2.2.2/32"] ["1.1.1.1/32"]
-- 0.5
shrinkRatio :: Foldable t
            => t a  -- ^ Old set
            -> t a  -- ^ New set
            -> Double    -- ^ Shrink ratio
shrinkRatio old new = 1 - (1 / (fromIntegral (length old) / fromIntegral (length new)))

-- |Parse our config file.  May throw AesonException on failure.
loadConfig :: FilePath -> IO TSConfig
loadConfig fp = LB.readFile fp >>= throwDecode

-- |Do all the hostname resolution and concat the results with static routes from
-- the config.
generateRoutes :: TSConfig -> IO (Set IPRange)
generateRoutes config = do
  hostIPs <- resolveHostnames (tsHostRoutes config)
  return $ S.fromList (tsRoutes config ++ map ipToHostRoute hostIPs)

-- |Given a ipv4 or ipv6 IP address, return a /32 or /128 CIDR route for it.
--
-- >>> ipToHostRoute (read "1.1.1.1" :: IP)
-- 1.1.1.1/32
--
-- >>> ipToHostRoute (read "fd00::1" :: IP)
-- fd00::1/128
ipToHostRoute :: IP -> IPRange
ipToHostRoute (IPv4 ip) = IPv4Range (makeAddrRange ip 32)
ipToHostRoute (IPv6 ip) = IPv6Range (makeAddrRange ip 128)

-- |Resolve a list of hostnames to a concatenated list of IPs.
resolveHostnames :: [HostName] -> IO [IP]
resolveHostnames hs = mapM resolveOne hs <&> concat . catMaybes

-- |Resolve one hostname to a list of IPs.
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

-- |Config file schema
data TSConfig
  = TSConfig
  { tsRoutes :: [IPRange]
  , tsHostRoutes :: [String]
  , tsExtraArgs :: [String]
  }
  deriving Show

-- |Config file JSON parser
instance FromJSON TSConfig where
  parseJSON = withObject "TSConfig" $ \obj -> do
    routes <- obj .:? "routes"
    hostRoutes <- obj .:? "hostRoutes"
    extraArgs <- obj .:? "extraArgs"
    return (TSConfig { tsRoutes = fromMaybe [] routes
                     , tsHostRoutes = fromMaybe [] hostRoutes
                     , tsExtraArgs = fromMaybe [] extraArgs
                     })

{-# LANGUAGE QuasiQuotes #-}

module TailscaleManager where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void, when)
import Control.Monad.Loops (iterateM_)
import Data.IP (IPRange)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Set qualified as S
import Options.Applicative
import Prettyprinter (Doc)
import System.Log.Logger
import System.Process (callProcess, showCommandForUser)
import Text.RawString.QQ (r)
import TailscaleManager.Config
import TailscaleManager.Discovery.AWSManagedPrefixList (resolveAllPrefixLists)
import TailscaleManager.Discovery.DNS (resolveHostnamesToRoutes)
import TailscaleManager.Logging (myLogger)

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

Config file example (JSON):

{
  "routes": [
    "172.16.0.0/22",
    "192.168.0.0/24"
  ],
  "hostRoutes": [
    "special-hostname1.example",
    "special-hostname2.example"
  ],
  "awsManagedPrefixLists": [
    "pl-02761f4a40454a3c9"
  ],
  "extraArgs": ["--webclient"]
}|]

tsManagerOptions :: Parser TailscaleManagerOptions
tsManagerOptions =
  TailscaleManagerOptions
  <$> argument str (metavar "<configfile>")
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
    then do
      logL logger INFO $ "Running every " ++ show (interval options) ++ " seconds"
      iterateM_ (runOnce options) S.empty
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
          logL logger DEBUG ("Sleeping for " ++ show (interval options) ++ " seconds")
        threadDelay (interval options * 1000000)  -- microseconds

  config <- TailscaleManager.Config.loadConfig (configFile options)
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

-- | Emit a log message describing the difference between old and new route sets.
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

-- | Compute how much smaller the new set is vs old.
shrinkRatio :: Foldable t => t a -> t a -> Double
shrinkRatio old new = 1 - (1 / (fromIntegral (length old) / fromIntegral (length new)))

-- | Generate routes based on config, resolving hostnames and AWS-managed prefix lists.
generateRoutes :: TSConfig -> IO (Set IPRange)
generateRoutes config = do
  hostRoutes <- resolveHostnamesToRoutes (tsHostRoutes config)
  managedPrefixRoutes <- resolveAllPrefixLists (tsAWSManagedPrefixLists config)
  return $ S.fromList (tsRoutes config <> hostRoutes <> managedPrefixRoutes)

-- | DNS hostname route discovery

module TailscaleManager.Discovery.DNS where

import Data.Functor ((<&>))
import Data.Maybe (catMaybes, mapMaybe)
import Control.Exception (IOException, try)
import Data.IP
import Network.Socket
import System.Log.Logger

import TailscaleManager.Logging

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

-- | Resolve hostnames to static routes
resolveHostnamesToRoutes :: [HostName] -> IO [IPRange]
resolveHostnamesToRoutes hs = resolveHostnames hs <&> map ipToHostRoute

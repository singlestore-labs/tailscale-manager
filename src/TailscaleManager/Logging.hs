-- | Logging setup

module TailscaleManager.Logging where

import Data.Functor ((<&>))
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Logger
import System.Log.Handler (LogHandler (setFormatter))
import System.Log.Handler.Simple (streamHandler)

myLogger :: IO Logger
myLogger = do
  let myFormatter = simpleLogFormatter "[$time $prio $loggername] $msg"
  handler <- streamHandler stderr INFO <&> flip setFormatter myFormatter
  getLogger "tailscale-manager" <&> setLevel INFO . setHandlers [handler]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
-- | AWS Managed Prefix List route discovery

module TailscaleManager.Discovery.AWSManagedPrefixList where

import Amazonka
import Amazonka.EC2
import Amazonka.Prelude (Text, mapMaybe, catMaybes)
import Control.Lens
import Data.Conduit
import Data.Text (unpack)
import qualified Data.Conduit.List as CL
import Data.Generics.Labels ()
import Data.IP (IPRange)
import System.IO (stderr)
import System.Log.Logger
import Text.Read (readMaybe)

import TailscaleManager.Logging

getPrefixListPaginated :: Text -- ^ prefix list ID
                       -> IO [PrefixListEntry]
getPrefixListPaginated pl = do
  -- TODO: figure out how to use my own HSLogger instance here instead of
  -- amazonka's built-in logger
  lgr <- newLogger Info stderr
  env <- newEnv discover <&> set #logger lgr

  runResourceT . runConduit $
    paginate env (newGetManagedPrefixListEntries pl)
      .| CL.concatMap (view $ #entries . _Just)
      .| CL.consume

-- | Look up a AWS Managed Prefix List by ID, returning parsed subnet prefixes.
-- Silently discards any unparsable prefixes, because it seems safe to
-- assume that AWS will return valid CIDR strings.
resolvePrefixListEntries :: Text  -- ^ Prefix list ID
                         -> IO [IPRange]
resolvePrefixListEntries pl =
  getPrefixListPaginated pl
    <&> mapMaybe (readMaybe . unpack . view (#cidr . _Just))

-- | Like 'resolvePrefixListEntries', but logs and discards errors.
resolvePrefixListEntries' :: Text  -- ^ Prefix list ID
                          -> IO (Maybe [IPRange])
resolvePrefixListEntries' pl = do
  lgr <- myLogger
  result <- trying _ServiceError $ resolvePrefixListEntries pl
  case result of
    Left err -> do
      -- TODO: better error messages
      logL lgr WARNING ("Failed to resolve a prefix list! " <> show err)
      return Nothing
    Right ipranges ->
      return $ Just ipranges

-- | Look up entries from multiple prefix lists, ignoring any errors
resolveAllPrefixLists :: [Text] -- ^ Prefix list IDs
                      -> IO [IPRange]
resolveAllPrefixLists pls =
  mapM resolvePrefixListEntries' pls <&> concat . catMaybes

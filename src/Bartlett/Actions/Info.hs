{-|
Module      : Info
Description : Methods for executing informational requests against Jenkins
Copyright   : (c) Nike, Inc., 2016
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Methods for executing informational requests against Jenkins.
-}
module Bartlett.Actions.Info (
  getInfo
) where

import Bartlett.Network (simpleErrorHandler)
import Bartlett.Types
import Bartlett.Util (toPrettyJson, mkUrl)

import qualified Control.Exception as E
import Control.Lens ((^.), (?~), (&))
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (getWith, responseBody, defaults, auth)


-- | Retrieve information for the given job from the given jenkins instance.
--
-- This method will contact Jenkins over the protocol specified by
-- 'JenkinsInstance'
getInfo ::
  BasicAuthUser b => b -- ^ The user to authenticate with.
  -> JenkinsInstance   -- ^ The Jenkins instance to authenticate against.
  -> [JobPath]         -- ^ The jobs to get information from.
  -> IO ()
getInfo user base [] = return ()
getInfo user base (path:paths) = do
  resp <- getWith reqOpts reqUri `E.catch` simpleErrorHandler
  BL.putStrLn . toPrettyJson $ resp ^. responseBody
  getInfo user base paths
    where reqOpts = defaults & auth ?~ getBasicAuth user
          reqUri  = BL.unpack (mkUrl base path "/api/json")

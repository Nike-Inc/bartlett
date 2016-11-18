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

import Bartlett.Network (execRequest)
import Bartlett.Types
import Bartlett.Util (toPrettyJson, mkUrl)

import Control.Lens ((^.), (?~), (&))
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (responseBody, defaults, auth)


-- | Retrieve information for the given job from the given jenkins instance.
--
-- This method will contact Jenkins over the protocol specified by
-- 'JenkinsInstance'. If not protocol is specified it will attempt to contact
-- Jenkins over SSL.
getInfo ::
  BasicAuthUser b => b -- ^ The user to authenticate with.
  -> JenkinsInstance   -- ^ The Jenkins instance to authenticate against.
  -> [JobPath]         -- ^ The jobs to get information from.
  -> IO ()
getInfo user base [] = return ()
getInfo user base (path:paths) = do
  resp <- execRequest Get reqOpts reqUri Nothing
  BL.putStrLn . toPrettyJson $ resp ^. responseBody
  getInfo user base paths
    where reqOpts = defaults & auth ?~ getBasicAuth user
          reqUri  = mkUrl base path "/api/json"

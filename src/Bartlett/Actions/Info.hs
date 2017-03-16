{-|
Module      : Info
Description : Methods for executing informational requests against Jenkins
Copyright   : (c) Nike, Inc., 2016-present
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

import Control.Lens (set, (^.), (&))
import Control.Monad.Reader (asks, liftIO)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (responseBody, defaults, auth)


-- | Retrieve information for the given job from the given jenkins instance.
--
-- This method will contact Jenkins over the protocol specified by
-- 'JenkinsInstance'. If not protocol is specified it will attempt to contact
-- Jenkins over SSL.
getInfo ::
  BasicAuthUser b => Maybe b -- ^ The user to authenticate with.
  -> [JobPath]               -- ^ The jobs to get information from.
  -> Bartlett ()
getInfo user [] = return ()
getInfo user (path:paths) = do
  jenkins <- fromJust <$> asks jenkinsInstance
  resp <- liftIO $ execRequest Get reqOpts (mkUrl jenkins path "/api/json") Nothing
  liftIO $ BL.putStrLn . toPrettyJson $ resp ^. responseBody
  getInfo user paths
    where reqOpts = defaults & set auth (getBasicAuth <$> user)

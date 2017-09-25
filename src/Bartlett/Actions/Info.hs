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

import           Bartlett.Network      (execRequest)
import           Bartlett.Types
import           Bartlett.Util         (mkUrl, toPrettyJson)

import           Control.Lens          (set, (&), (^.))
import           Control.Monad.Reader  (asks, liftIO)
import qualified Data.ByteString.Char8 as BL
import           Data.Maybe            (fromJust)
import           Network.Wreq          (auth, defaults, responseBody)


-- | Retrieve information for the given job from the given jenkins instance.
--
-- This method will contact Jenkins over the protocol specified by
-- 'JenkinsInstance'. If not protocol is specified it will attempt to contact
-- Jenkins over SSL.
getInfo ::
  BasicAuthUser b => Maybe b -- ^ The user to authenticate with.
  -> [JobPath]               -- ^ The jobs to get information from.
  -> Bartlett (Either BartlettError ())
getInfo user [] = return . Right $ ()
getInfo user (path:paths) =
  let reqOpts = defaults & set auth (getBasicAuth <$> user)
  in do
    jenkins <- fromJust <$> asks jenkinsInstance
    response <- execRequest Get reqOpts (mkUrl jenkins path "/api/json") Nothing
    case response of
      Left e ->
        return . Left $ e
      Right resp -> do
        liftIO $ BL.putStrLn . toPrettyJson $ resp ^. responseBody
        getInfo user paths

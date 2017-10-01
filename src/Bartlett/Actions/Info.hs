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

import           Bartlett.Types
import           Bartlett.Util         (mkUrl, toPrettyJson, withJenkins)

import           Control.Lens          (set, (&), (^.))
import           Control.Monad.Reader  (ask, liftIO)
import qualified Data.ByteString.Char8 as BL
import           Network.Wreq          (auth, defaults, responseBody)


-- | Retrieve information for the given job from the given jenkins instance.
--
-- This method will contact Jenkins over the protocol specified by
-- 'JenkinsInstance'. If not protocol is specified it will attempt to contact
-- Jenkins over SSL.
getInfo ::
  [JobPath]               -- ^ The jobs to get information from.
  -> Bartlett (Either BartlettError ())
getInfo [] = return . Right $ ()
getInfo (path:paths) = do
    options <- ask
    let reqOpts = defaults & set auth (getBasicAuth <$> user options)
    let execRequest = requestExecutor options

    withJenkins (mkUrl (jenkinsInstance options) path "/api/json") $ do
      response <- execRequest Get reqOpts Nothing
      case response of
        Left e ->
          return . Left $ e
        Right resp -> do
          liftIO $ BL.putStrLn . toPrettyJson $ resp ^. responseBody
          getInfo paths

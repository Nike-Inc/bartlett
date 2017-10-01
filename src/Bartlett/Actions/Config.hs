{-|
Module      : Config
Description : Methods for executing config requests against Jenkins
Copyright   : (c) Nike, Inc., 2016-present
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Methods for executing config requests against Jenkins.
-}
module Bartlett.Actions.Config where

import           Bartlett.Types
import           Bartlett.Util            (mkUrl, toResponseStatus, withJenkins)

import           Control.Lens             (set, (&), (^.))
import           Control.Monad.Reader     (ask, liftIO)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as Lazy
import           Network.Wreq             (auth, defaults, responseBody,
                                           responseStatus)

-- | Construct a URL to interact with Job configurations.
configUri :: JenkinsInstance -> JobPath -> JenkinsInstance
configUri base path =
  mkUrl base path "/config.xml"

-- | Retrieve the XML configuration for the given job.
getConfig :: JobPath -> Bartlett (Either BartlettError ())
getConfig path = do
  options <- ask
  let reqOpts = defaults & set auth (getBasicAuth <$> user options)
  let execRequest = requestExecutor options

  withJenkins (configUri (jenkinsInstance options) path) $ do
    response <- execRequest Get reqOpts Nothing
    case response of
      Left e ->
        return . Left $ e
      Right resp -> do
        liftIO $ BC.putStrLn $ resp ^. responseBody
        return . Right $ ()

-- | Update the XML configuration for the given job.
updateConfig ::
  JobPath       -- ^ The Job for the given Jenkins instance to interact with.
  -> ConfigPath -- ^ Path to the XML configuration to upload to Jenkins.
  -> Bartlett (Either BartlettError ())
updateConfig path configPath = do
  configFile <- liftIO $ BC.readFile configPath
  options <- ask
  let reqOpts = defaults & set auth (getBasicAuth <$> user options)
  let execRequest = requestExecutor options

  withJenkins (configUri (jenkinsInstance options) path) $ do
    response <- execRequest Post reqOpts (Just configFile)
    case response of
      Left e ->
        return . Left $ e
      Right resp -> do
        liftIO $ BC.putStrLn
          . Lazy.toStrict
          . encodePretty
          . toResponseStatus $ resp ^. responseStatus
        return . Right $ ()

-- | Delete the XML configuration for the given job.
deleteConfig ::
  [JobPath] -- ^ The job for the given Jenkins instance to delete.
  -> Bartlett (Either BartlettError ())
deleteConfig [] = return . Right $ ()
deleteConfig (path:paths) = do
  options <- ask
  let reqOpts = defaults & set auth (getBasicAuth <$> user options)
  let execRequest = requestExecutor options

  withJenkins (mkUrl (jenkinsInstance options) path "/doDelete") $ do
    response <- execRequest Post reqOpts Nothing
    case response of
      Left e ->
        return . Left $ e
      Right resp -> do
        liftIO $ BC.putStrLn
          . Lazy.toStrict
          . encodePretty
          . toResponseStatus $ resp ^. responseStatus
        deleteConfig paths

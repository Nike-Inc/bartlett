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

import Bartlett.Network (execRequest)
import Bartlett.Types
import Bartlett.Util (toResponseStatus, mkUrl)

import Control.Lens (set, (^.), (&))
import Data.Maybe (Maybe, fromJust)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (responseStatus, responseBody, defaults, auth)
import Control.Monad.Reader (liftIO, asks)

-- | Construct a URL to interact with Job configurations.
configUri :: JenkinsInstance -> JobPath -> JenkinsInstance
configUri base path =
  mkUrl base path "/config.xml"

-- | Retrieve the XML configuration for the given job.
getConfig :: BasicAuthUser a => Maybe a -> JobPath -> Bartlett ()
getConfig user path = do
  jenkins <- fromJust <$> asks jenkinsInstance
  resp <- liftIO $ execRequest Get reqOpts (configUri jenkins path) Nothing
  liftIO $ BL.putStrLn $ resp ^. responseBody
    where reqOpts = defaults & set auth (getBasicAuth <$> user)

-- | Update the XML configuration for the given job.
updateConfig :: BasicAuthUser a =>
  Maybe a            -- The user to authenticate with.
  -> JobPath         -- The Job for the given Jenkins instance to interact with.
  -> ConfigPath      -- Path to the XML configuration to upload to Jenkins.
  -> Bartlett ()
updateConfig user path configPath = do
  jenkins <- fromJust <$> asks jenkinsInstance
  configFile <- liftIO $ BL.readFile configPath
  resp <- liftIO $ execRequest Post reqOpts (configUri jenkins path) (Just configFile)
  liftIO $ BL.putStrLn . encodePretty . toResponseStatus $ resp ^. responseStatus
    where reqOpts = defaults & set auth (getBasicAuth <$> user)

-- | Delete the XML configuration for the given job.
deleteConfig :: BasicAuthUser a =>
  Maybe a              -- The user to authenticate with.
  -> [JobPath]         -- The job for the given Jenkins instance to delete.
  -> Bartlett ()
deleteConfig user [] = return ()
deleteConfig user (path:paths) = do
  jenkins <- fromJust <$> asks jenkinsInstance
  resp <- liftIO $ execRequest Post reqOpts (mkUrl jenkins path "/doDelete") Nothing
  liftIO $ BL.putStrLn . encodePretty . toResponseStatus $ resp ^. responseStatus
  deleteConfig user paths
    where reqOpts = defaults & set auth (getBasicAuth <$> user)

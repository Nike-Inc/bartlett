{-|
Module      : Config
Description : Methods for executing config requests against Jenkins
Copyright   : (c) Nike, Inc., 2016
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
import Data.Maybe (Maybe)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (responseStatus, responseBody, defaults, auth)

-- | Construct a URL to interact with Job configurations.
configUri :: JenkinsInstance -> JobPath -> JenkinsInstance
configUri base path =
  mkUrl base path "/config.xml"

-- | Retrieve the XML configuration for the given job.
getConfig :: BasicAuthUser a =>
  Maybe a            -- The user to authenticate with.
  -> JenkinsInstance -- The Jenkins instance to interact with.
  -> JobPath         -- The Job for the given Jenkins instance to interact with.
  -> IO ()           -- The XML configuration for the given job.
getConfig user base path = do
  resp <- execRequest Get reqOpts (configUri base path) Nothing
  BL.putStrLn $ resp ^. responseBody
    where reqOpts = defaults & set auth (getBasicAuth <$> user)

-- | Update the XML configuration for the given job.
updateConfig :: BasicAuthUser a =>
  Maybe a            -- The user to authenticate with.
  -> JenkinsInstance -- The Jenkins instance to interact with.
  -> JobPath         -- The Job for the given Jenkins instance to interact with.
  -> ConfigPath      -- Path to the XML configuration to upload to Jenkins.
  -> IO ()
updateConfig user base path configPath = do
  configFile <- BL.readFile configPath
  resp <- execRequest Post reqOpts (configUri base path) (Just configFile)
  BL.putStrLn . encodePretty . toResponseStatus $ resp ^. responseStatus
    where reqOpts = defaults & set auth (getBasicAuth <$> user)

deleteConfig :: BasicAuthUser a =>
  Maybe a              -- The user to authenticate with.
  -> JenkinsInstance   -- The Jenkins instance to interact with.
  -> [JobPath]         -- The job for the given Jenkins instance to delete.
  -> IO ()
deleteConfig user base [] = return ()
deleteConfig user base (path:paths) = do
  resp <- execRequest Post reqOpts (mkUrl base path "/doDelete") Nothing
  BL.putStrLn . encodePretty . toResponseStatus $ resp ^. responseStatus
  deleteConfig user base paths
    where reqOpts = defaults & set auth (getBasicAuth <$> user)

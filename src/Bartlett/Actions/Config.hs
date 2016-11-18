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

import Control.Lens ((^.), (?~), (&))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (responseStatus, responseBody, defaults, auth)

-- | Construct a URL to interact with Job configurations.
configUri :: JenkinsInstance -> JobPath -> BL.ByteString
configUri base path =
  mkUrl base path "/config.xml"

-- | Retrieve the XML configuration for the given job.
getConfig :: BasicAuthUser a =>
  a                  -- The user to authenticate with.
  -> JenkinsInstance -- The Jenkins instance to interact with.
  -> JobPath         -- The Job for the given Jenkins instance to interact with.
  -> IO ()           -- The XML configuration for the given job.
getConfig user base path = do
  resp <- execRequest Get reqOpts (configUri base path) Nothing
  BL.putStrLn $ resp ^. responseBody
    where reqOpts = defaults & auth ?~ getBasicAuth user

-- | Update the XML configuration for the given job.
updateConfig :: BasicAuthUser a =>
  a                  -- The user to authenticate with.
  -> JenkinsInstance -- The Jenkins instance to interact with.
  -> JobPath         -- The Job for the given Jenkins instance to interact with.
  -> ConfigPath      -- Path to the XML configuration to upload to Jenkins.
  -> IO ()
updateConfig user base path configPath = do
  configFile <- BL.readFile configPath
  resp <- execRequest Post reqOpts (configUri base path) (Just configFile)
  BL.putStrLn . encodePretty . toResponseStatus $ resp ^. responseStatus
    where reqOpts = defaults & auth ?~ getBasicAuth user

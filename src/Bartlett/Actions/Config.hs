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
  a
  -> JenkinsInstance
  -> JobPath
  -> IO ()
getConfig user base path = do
  resp <- execRequest "get" reqOpts (configUri base path) Nothing
  BL.putStrLn $ resp ^. responseBody
    where reqOpts = defaults & auth ?~ getBasicAuth user

-- | Update the XML configuration for the given job.
updateConfig :: BasicAuthUser a =>
  a
  -> JenkinsInstance
  -> JobPath
  -> ConfigPath
  -> IO ()
updateConfig user base path configPath = do
  configFile <- BL.readFile configPath
  resp <- execRequest "post" reqOpts (configUri base path) (Just configFile)
  BL.putStrLn . encodePretty . toResponseStatus $ resp ^. responseStatus
    where reqOpts = defaults & auth ?~ getBasicAuth user

{-|
Module      : Configuration
Description : Configuration management for Bartlett
Copyright   : (c) Nike, Inc., 2016
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Configuration management for Bartlett.
-}
module Bartlett.Configuration (
  -- * Configuration Management
  defaultConfigLoc,
  getConfiguration,
  -- * Convenience Accessors
  getUsername,
  getJenkinsInstance,
  getStorePassword
) where

import Prelude hiding (concat)

import Bartlett.Util (toText)
import Bartlett.Types

import Data.ByteString.Lazy.Char8
import qualified Data.Configurator as C
import Data.Configurator.Types
import System.FilePath ((</>))

-- | Default config file location
defaultConfigLoc :: FilePath
defaultConfigLoc = "$(HOME)" </> ".bartlett.cfg"

-- | Attempt to retrieve the default configuration.
--
--   Returns an empty configuration if it could not load the default.
getConfiguration :: Profile -> IO Config
getConfiguration profile =
  C.subconfig (toText profile) <$> C.load [Optional defaultConfigLoc]

-- | Retrieve the username for the given profile.
getUsername :: Config -> IO (Maybe Username)
getUsername cfg =
  C.lookup cfg (toText "username")

-- | Retrieve the Jenkins instance for the given profile.
getJenkinsInstance :: Config -> IO (Maybe JenkinsInstance)
getJenkinsInstance cfg =
  C.lookup cfg (toText "jenkins_instance")

-- | Get the value determining whether the user's password should be stored.
getStorePassword :: Config -> IO (Maybe Bool)
getStorePassword cfg =
  C.lookup cfg (toText "store_password")


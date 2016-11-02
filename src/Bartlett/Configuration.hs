{-|
Module      : Configuration
Description : Configuration management for Bartlett
Copyright   : (c) Nike, Inc., 2016
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Configuration management for Bartlett.
-}
module Bartlett.Configuration where

import Prelude hiding (concat)

import Bartlett.Util (toText)

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
getConfiguration :: IO Config
getConfiguration = C.load [Optional defaultConfigLoc]

-- | Retrieve the value from the provided profile and configuration source.
getValueFromConfiguration ::
  ByteString               -- ^ The profile to retrieve values from.
  -> ByteString            -- ^ The value to retrieve from the given profile.
  -> Config                -- ^ The configuration source to retrieve values from.
  -> IO (Maybe ByteString) -- ^ Just the value if it was found, Nothing otherwise.
getValueFromConfiguration profile fieldName cfg =
  C.lookup cfg qualifiedFieldName :: IO (Maybe ByteString)
    where qualifiedFieldName = toText (concat [profile, ".", fieldName])

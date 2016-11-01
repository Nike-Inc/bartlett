{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Network
Description : General network related methods used throughout Bartlett
Copyright   : (c) Nike, Inc., 2016
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable
General network related methods used throughout Bartlett.
-}
module Bartlett.Network (
  simpleErrorHandler
)where

import Bartlett.Util (toResponseStatus)

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Network.HTTP.Client as NHC
import System.Exit (die)

-- | Handler that returns a JSON representation of the error status.
simpleErrorHandler :: NHC.HttpException -> IO a
simpleErrorHandler e@(NHC.StatusCodeException status _ _) =
  die . unpack . encodePretty . toResponseStatus $ status

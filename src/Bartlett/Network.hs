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
  -- * Request handlers
  execRequest,
  -- * Error Handlers
  simpleErrorHandler,
  recoverableErrorHandler
)where

import Bartlett.Util (toResponseStatus, withForcedSSL)

import qualified Control.Exception as E
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import qualified Network.HTTP.Client as NHC
import System.Exit (die)
import Network.Wreq (Options, Response, customMethodWith)

execRequest :: ByteString -> Options -> ByteString -> IO (Response ByteString)
execRequest requestType opts reqUrl =
  case requestType of
    "post" ->
      mkRequest "post" reqUrl
        `E.catch`
          recoverableErrorHandler (mkRequest "post" $ withForcedSSL reqUrl)
    "get" ->
      mkRequest "get" reqUrl
        `E.catch`
          recoverableErrorHandler (mkRequest "get" $ withForcedSSL reqUrl)

    where mkRequest method url = customMethodWith method opts (unpack url)


-- | Handler that returns a JSON representation of the error status.
simpleErrorHandler :: NHC.HttpException -> IO a
simpleErrorHandler e@(NHC.StatusCodeException status _ _) =
  die . unpack . encodePretty . toResponseStatus $ status

-- | Attempt to recover from non-fatal errors with the provided action, otherwise
--   fail again with the 'simpleErrorHandler'
recoverableErrorHandler :: IO a -> NHC.HttpException -> IO a
recoverableErrorHandler a e =
  case e of
    (NHC.InvalidUrlException _ _) ->
      -- Retry with the given IO action
      a `E.catch` simpleErrorHandler
    _ -> -- Otherwise fall through to the simple error handler
      simpleErrorHandler e

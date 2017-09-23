{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Util
Description : Utility methods used throughout Bartlett
Copyright   : (c) Nike, Inc., 2016-present
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

A collection of utility methods used throughout Bartlett.
-}
module Bartlett.Util (
  -- * URL Helpers
  setPath,
  mkUrl,
  mkJobPath,
  withForcedSSL,
  segmentPath,
  -- * Type Conversions
  toPrettyJson,
  toResponseStatus,
  uriToString,
  -- * Query Parameter Helpers
  parametersBuilder,
  optionsBuilder
)where

import           Prelude                    hiding (dropWhile, null)

import           Bartlett.Types

import           Control.Lens               (set, (^.))
import           Data.Aeson                 (Object, decode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString.Builder    (toLazyByteString)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Network.HTTP.Types.Status
import qualified Network.Wreq               as W
import           URI.ByteString             (pathL, schemeBSL, serializeURIRef,
                                             uriSchemeL)

setPath :: JenkinsInstance -> ByteString -> JenkinsInstance
setPath jenkins path =
  set pathL (BS.toStrict path) jenkins

-- | Constructs a valid Jenkins API url.
mkUrl :: JenkinsInstance -> JobPath -> ByteString -> JenkinsInstance
mkUrl base path suffix =
  setPath base (mkJobPath path <> suffix)

-- | Given a slash-delimited path, return that same path interspersed with '/job/'.
mkJobPath :: JobPath -> ByteString
mkJobPath ""  = ""
mkJobPath "/" = ""
mkJobPath s   = BS.append "/job/" . BS.intercalate "/job/" . segmentPath $ s

-- | Given a base Jenkins instance, force the use of HTTPS
withForcedSSL :: JenkinsInstance -> JenkinsInstance
withForcedSSL base =
  case base ^. uriSchemeL . schemeBSL of
    "http" ->
      set (uriSchemeL . schemeBSL) "https" base
    _ ->
      base

-- | Segment a slash-delimited string as well as filter empty elements.
segmentPath :: ByteString -> [ByteString]
segmentPath = filter (not . BS.null) . BS.split '/'

-- | Return a pretty-formatted JSON string
toPrettyJson :: ByteString -> ByteString
toPrettyJson s = encodePretty (decode s :: Maybe Object)

-- | Translation from Wreq's 'Status' to "StatusResponse".
toResponseStatus :: Status -> StatusResponse
toResponseStatus (Status code msg) =
  StatusResponse {
    statusCode = code,
    statusMessage = (BS.unpack . BS.fromStrict) msg
  }

-- | Serialize a URI to a String
uriToString :: JenkinsInstance -> String
uriToString = BS.unpack . toLazyByteString . serializeURIRef

-- | Compose a list of key=val pairs into a "Network.Wreq.Options" builder.
parametersBuilder :: [(Text, Text)] -> (W.Options -> W.Options)
parametersBuilder opts = Prelude.foldl (.) id $
  fmap (\ (k, v) -> set (W.param k) [v]) opts

-- | Compose the provided options builder with Wreq's default options.
optionsBuilder :: (W.Options -> W.Options) -> W.Options
optionsBuilder builder = builder W.defaults

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

import           Bartlett.Types

import           Control.Lens              (Getting)
import qualified Control.Lens              as Lens
import           Data.Aeson                (Object, decodeStrict')
import           Data.Aeson.Encode.Pretty  (encodePretty)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as Lazy
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import           Network.HTTP.Types.Status
import qualified Network.Wreq              as W
import           URI.ByteString            (pathL, schemeBSL, serializeURIRef',
                                            uriSchemeL)

-- | Gets the value of the provided 'Lens'.
--
-- Internal helper to avoid syntax like (^.)
get :: s -> Getting a s a -> a
get = flip Lens.view

setPath :: JenkinsInstance -> ByteString -> JenkinsInstance
setPath jenkins path =
  Lens.set pathL path jenkins

-- | Constructs a valid Jenkins API url.
mkUrl :: JenkinsInstance -> JobPath -> ByteString -> JenkinsInstance
mkUrl base path suffix =
  setPath base (mkJobPath path <> suffix)

-- | Given a slash-delimited path, return that same path interspersed with '/job/'.
mkJobPath :: JobPath -> ByteString
mkJobPath ""  = ""
mkJobPath "/" = ""
mkJobPath s   = B.append "/job/" . B.intercalate "/job/" . segmentPath $ s

-- | Given a base Jenkins instance, force the use of HTTPS
withForcedSSL :: JenkinsInstance -> JenkinsInstance
withForcedSSL base =
  case base `get` (uriSchemeL . schemeBSL) of
    "http" ->
      Lens.set (uriSchemeL . schemeBSL) "https" base
    _ ->
      base

-- | Segment a slash-delimited string as well as filter empty elements.
segmentPath :: ByteString -> [ByteString]
segmentPath = filter (not . B.null) . BC.split '/'

-- | Return a pretty-formatted JSON string
toPrettyJson :: ByteString -> ByteString
toPrettyJson s = Lazy.toStrict $ encodePretty (decodeStrict' s :: Maybe Object)

-- | Translation from Wreq's 'Status' to "StatusResponse".
toResponseStatus :: Status -> StatusResponse
toResponseStatus (Status code msg) =
  StatusResponse {
    statusCode = code,
    statusMessage = BC.unpack msg
  }

-- | Serialize a URI to a String
uriToString :: JenkinsInstance -> String
uriToString = BC.unpack . serializeURIRef'

-- | Compose a list of key=val pairs into a "Network.Wreq.Options" builder.
parametersBuilder :: [(Text, Text)] -> (W.Options -> W.Options)
parametersBuilder = foldl (.) id . fmap (\ (k, v) -> Lens.set (W.param k) [v])

-- | Compose the provided options builder with Wreq's default options.
optionsBuilder :: (W.Options -> W.Options) -> W.Options
optionsBuilder builder = builder W.defaults

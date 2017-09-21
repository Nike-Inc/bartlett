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
  pairToTuple,
  -- * Type Conversions
  toByteString,
  toPrettyJson,
  toResponseStatus,
  uriToString,
  -- * Query Parameter Helpers
  parseParameters,
  parametersBuilder,
  optionsBuilder
)where

import           Prelude                    hiding (dropWhile, null)

import           Bartlett.Types

import           Control.Lens               (set, (^.))
import           Data.Aeson                 (Object, decode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString.Builder    (toLazyByteString)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid                ((<>))
import Data.Text (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
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

-- | A non-total function converting a list pair to a proper tuple.
pairToTuple :: [a] -> (a, a)
pairToTuple []     = error "Attempted to convert empty list to a 2-tuple."
pairToTuple [a, b] = (a, b)
pairToTuple _      = error "Attempted to convert a list of size != 2 to a 2-tuple."

-- | Convert a "Text" to a "ByteString"
toByteString :: T.Text -> ByteString
toByteString = BS.fromStrict . TE.encodeUtf8

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

-- | Given a comma delimited list of key=value pairs, return a collection
-- of pairs.
parseParameters :: Text -> [(Text, Text)]
parseParameters "" = []
parseParameters t = fmap (pairToTuple . T.splitOn "=") . T.splitOn "," $ t

-- | Compose a list of key=val pairs into a "Network.Wreq.Options" builder.
parametersBuilder :: [(Text, Text)] -> (W.Options -> W.Options)
parametersBuilder opts = Prelude.foldl (.) id $
  fmap (\ (k, v) -> set (W.param k) [v]) opts

-- | Compose the provided options builder with Wreq's default options.
optionsBuilder :: (W.Options -> W.Options) -> W.Options
optionsBuilder builder = builder W.defaults

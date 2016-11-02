{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Util
Description : Utility methods used throughout Bartlett
Copyright   : (c) Nike, Inc., 2016
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

A collection of utility methods used throughout Bartlett.
-}
module Bartlett.Util (
  -- * URL Helpers
  -- $urlHelpers
  mkUrl,
  mkJobPath,
  withForcedSSL,
  segmentPath,
  pairToTuple,
  -- * Type Conversions
  -- $typeConversions
  toText,
  toPrettyJson,
  toResponseStatus,
  -- * Query Parameter Helpers
  -- $queryParameterHelpers
  parseParameters,
  parametersBuilder,
  optionsBuilder
)where

import Prelude hiding (concat, null, dropWhile)

import Bartlett.Types

import Control.Lens (set)
import Data.Aeson (decode, Object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Wreq as W
import Network.HTTP.Types.Status

-- $urlHelpers
-- Helper methods for dealing with URLs.

-- | Constructs a valid Jenkins API url.
mkUrl :: JenkinsInstance -> JobPath -> ByteString -> ByteString
mkUrl base path suffix = concat [base, mkJobPath path, suffix]

-- | Given a slash-delimited path, return that same path interspersed with '/job/'.
mkJobPath :: ByteString -> ByteString
mkJobPath ""  = ""
mkJobPath "/" = ""
mkJobPath s   = append "/job/" . intercalate "/job/" . segmentPath $ s

-- | Given a base Jenkins instance, force the use of HTTPS
withForcedSSL :: JenkinsInstance -> JenkinsInstance
withForcedSSL base =
  if "http://" `isPrefixOf` base || "https://" `isPrefixOf` base
     then concat ["https", dropWhile (/=':') base]
     else concat ["https://", base]

-- | Segment a slash-delimited string as well as filter empty elements.
segmentPath :: ByteString -> [ByteString]
segmentPath = Prelude.filter (not . null) . split '/'

-- | A non-total function converting a list pair to a proper tuple.
pairToTuple :: [a] -> (a, a)
pairToTuple []     = error "Attempted to convert empty list to a 2-tuple."
pairToTuple [a, b] = (a, b)
pairToTuple _      = error "Attempted to convert a list of size != 2 to a 2-tuple."

-- $typeConversions
-- Helpers methods for converting between types.

-- | Convert a lazy "ByteString" to "Text".
toText :: ByteString -> T.Text
toText = TE.decodeUtf8 . toStrict

-- | Return a pretty-formatted JSON string
toPrettyJson :: ByteString -> ByteString
toPrettyJson s = encodePretty (decode s :: Maybe Object)

-- | Translation from Wreq's 'Status' to "StatusResponse".
toResponseStatus :: Status -> StatusResponse
toResponseStatus (Status code msg) =
  StatusResponse {
    statusCode = code,
    statusMessage = (unpack . fromStrict) msg
  }

-- $queryParameterHelpers
-- Helpers for parsing query parameters obtained from the command line and
-- converting them to formats that 'Wreq' supports.

-- | Given a comma delimited list of key=value pairs, return a collection
-- of pairs.
parseParameters :: ByteString -> [(ByteString, ByteString)]
parseParameters = fmap (pairToTuple . split '=') . split ','

-- | Compose a list of key=val pairs into a "Network.Wreq.Options" builder.
parametersBuilder :: [(ByteString, ByteString)] -> (W.Options -> W.Options)
parametersBuilder opts = Prelude.foldl (.) id $
  fmap (\ (k, v) -> set (W.param $ toText k) [toText v]) opts

-- | Compose the provided options builder with Wreq's default options.
optionsBuilder :: (W.Options -> W.Options) -> W.Options
optionsBuilder builder = builder W.defaults

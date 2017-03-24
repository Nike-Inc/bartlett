{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Network
Description : General network related methods used throughout Bartlett
Copyright   : (c) Nike, Inc., 2016-present
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable
General network related methods used throughout Bartlett.
-}
module Bartlett.Network (
  -- * Request handlers
  requestCSRFToken,
  consCSRFHeader,
  execRequest,
  -- * Error Handlers
  simpleErrorHandler,
  recoverableErrorHandler
)where

import           Bartlett.Types             (JenkinsInstance, RequestType (..))
import qualified Bartlett.Util              as BU

import qualified Control.Exception          as E
import           Control.Lens               ((&), (.~), (^?))
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Aeson.Lens            (key, _String)
import           Data.ByteString.Lazy.Char8 (ByteString, empty, toStrict,
                                             unpack)
import qualified Data.CaseInsensitive       as CI
import           Data.Maybe                 (fromMaybe)
import qualified Network.HTTP.Client        as NHC
import           Network.Wreq               (Options, Response, header, param,
                                             responseBody)
import qualified Network.Wreq.Session       as S
import           System.Exit                (die)


-- | Attempt to request a CSRF token from the Jenkins server.
requestCSRFToken ::
  S.Session                                  -- The current session used to interact with Jenkins.
  -> Options                                 -- Request parameters to pass along with the request.
  -> JenkinsInstance                         -- The uri to make the request to
  -> IO (Maybe ByteString, Maybe ByteString) -- The CSRF crumb to attach to future requests.
requestCSRFToken sess opts jenkins = do
  -- TODO fix this ugly mess
  resp <- E.try (S.getWith reqOpts sess (BU.uriToString reqUri)) :: IO (Either NHC.HttpException (Response ByteString))
  case resp of
    Left _ ->
      return (Nothing, Nothing)
    Right r ->
      return
        (BU.toByteString <$> (r ^? responseBody . key (BU.toText "crumbRequestField") . _String),
         BU.toByteString <$> (r ^? responseBody . key (BU.toText "crumb") . _String))
  where reqUri = BU.setPath jenkins "/crumbIssuer/api/json"
        reqOpts = opts & param "xpath" .~ [BU.toText "concat(//crumbRequestField,\":\",//crumb)"]

-- | Construct a valid header from a potential CSRF response.
consCSRFHeader :: IO (Maybe ByteString, Maybe ByteString) -> IO (Options -> Options)
consCSRFHeader ioCrumb = ioCrumb >>= \ (field, crumb) ->
  return $ header (CI.mk . toStrict . fromMaybe empty $ field) .~ [(toStrict . fromMaybe empty) crumb]


-- | General request handler that provides basic error handling.
execRequest ::
  RequestType                 -- ^ The type of request to make
  -> Options                  -- ^ Request params to pass along with the request.
  -> JenkinsInstance          -- ^ The uri to make the request to
  -> Maybe ByteString         -- ^ The file to upload to the Jenkins instance.
  -> IO (Response ByteString) -- ^ The response from the Jenkins instance.
execRequest requestType reqOpts reqUrl postBody =
  S.withAPISession $ \session ->
    case requestType of
      Post -> do
        csrfCrumb <- consCSRFHeader $ requestCSRFToken session reqOpts reqUrl
        postSession reqUrl (reqOpts & csrfCrumb)
          `E.catch`
            recoverableErrorHandler (postSession (BU.withForcedSSL reqUrl) (reqOpts & csrfCrumb))
              where fileToUpload = fromMaybe "" postBody :: ByteString
                    postSession url opts = S.postWith opts session (BU.uriToString url) fileToUpload
      Get ->
        getSession reqUrl
          `E.catch`
            recoverableErrorHandler (getSession . BU.withForcedSSL $ reqUrl)
              where getSession url = S.getWith reqOpts session (BU.uriToString url)


-- | Handler that returns a JSON representation of the error status.
simpleErrorHandler :: NHC.HttpException -> IO a
simpleErrorHandler (NHC.HttpExceptionRequest _ (NHC.StatusCodeException resp _)) =
  die . unpack . encodePretty . BU.toResponseStatus . NHC.responseStatus $ resp

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

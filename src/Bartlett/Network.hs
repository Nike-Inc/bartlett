{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}

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
  -- * CSRF Management
  requestCSRFToken,
  consCSRFHeader,
  -- * Network Handlers
  execRequest
)where

import           Bartlett.Types
import qualified Bartlett.Util          as BU

import qualified Control.Exception      as E
import           Control.Lens           ((&), (.~), (^.))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Data.Aeson             (decode)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.CaseInsensitive   as CI
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE
import           Network.HTTP.Client    (HttpException)
import           Network.Wreq           (Options, Response, header, param,
                                         responseBody)
import qualified Network.Wreq.Session   as S

-- | Attempt to request a CSRF token from the Jenkins server.
requestCSRFToken ::
  S.Session          -- ^ The current session used to interact with Jenkins.
  -> Options         -- ^ Request parameters to pass along with the request.
  -> JenkinsInstance -- ^ The uri to make the request to
  -> IO (Either BartlettError CrumbResponse) -- ^ Error if unable to acquire CSRF crumb, otherwise the CSRF crumb to attach to future requests.
requestCSRFToken sess opts jenkins = do
  resp <- E.try (S.getWith reqOpts sess (BU.uriToString reqUri)) :: IO (Either HttpException _)
  case resp of
    Left _ ->
      return . Left $ CsrfProtectionNotEnabled
    Right r ->
      return . Right . fromJust . decode $ r ^. responseBody
  where reqUri = BU.setPath jenkins "/crumbIssuer/api/json"
        reqOpts = opts & param "xpath" .~ ["concat(//crumbRequestField,\":\",//crumb)"]

-- | Construct a valid header from a CSRF key/value pair.
consCSRFHeader :: (Text, Text) -> (Options -> Options)
consCSRFHeader (field, crumb) =
  header (CI.mk . TE.encodeUtf8 $ field) .~ [TE.encodeUtf8 crumb]

-- | General request handler that provides basic error handling.
execRequest ::
  RequestType         -- ^ The type of request to make
  -> Options          -- ^ Request params to pass along with the request.
  -> Maybe ByteString -- ^ The file to upload to the Jenkins instance.
  -> Bartlett (Either BartlettError (Response ByteString)) -- ^ The response from the Jenkins instance.
execRequest requestType reqOpts postBody = do
    reqUrl <- asks jenkinsInstance

    let stringUri = BU.uriToString reqUrl

    liftIO $ S.withAPISession $ \session ->
      case requestType of
        Post ->
          let fileToUpload = fromMaybe "" postBody
              postSession opts = S.postWith opts session stringUri fileToUpload
              -- If CSRF protection is on, get a token and add it to our opts,
              -- otherwise return normal opts
              getOpts = do
                csrfCrumb <- requestCSRFToken session reqOpts reqUrl
                case csrfCrumb of
                  Right c ->
                    return $ reqOpts & consCSRFHeader (crumbRequestField c, crumb c)
                  Left _ ->
                    return reqOpts
          in do
          opts <- getOpts
          postResponse <- E.try (postSession opts) :: IO (Either HttpException _)
          case postResponse of
            Left _ ->
              -- TODO add more complete types and error messages here
              return . Left $ NetworkError
            Right resp ->
              return . Right $ Lazy.toStrict <$> resp
        Get -> do
          getResponse <- E.try (S.getWith reqOpts session stringUri) :: IO (Either HttpException _)
          case getResponse of
            Left _ ->
              -- TODO add more complete types and error messages here
              return . Left $ NetworkError
            Right resp ->
              return . Right $ Lazy.toStrict <$> resp

-- TODO given the code repetition above for handling errors we may want
-- to re-implement an error handler to reduce bloat.

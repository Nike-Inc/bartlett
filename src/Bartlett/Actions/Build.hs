{-|
Module      : Build
Description : Methods for executing build requests against Jenkins
Copyright   : (c) Nike, Inc., 2016-present
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Methods for executing build requests against Jenkins.
-}
module Bartlett.Actions.Build (
  postBuild
) where

import           Bartlett.Network           (execRequest)
import           Bartlett.Types
import qualified Bartlett.Util              as BU
import Bartlett.Actions.Log as BL

import           Data.Aeson.Lens            (key, _String)
import           Data.Monoid                ((<>))
import           Control.Lens               (set, (&), (^.), (^?))
import           Control.Monad.Reader       (asks, liftIO)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                 (fromJust)
import           Network.Wreq               (Options, auth, responseStatus, defaults, responseBody)


-- | Parses and determines type of job to trigger based on supplied parameters.
consBuildType :: Maybe JobParameters -> (BL.ByteString, Network.Wreq.Options)
consBuildType Nothing =
  ("/build", BU.optionsBuilder (BU.parametersBuilder [("", "")]))
consBuildType (Just jobParameters) =
  ("/buildWithParameters",
    (BU.optionsBuilder . BU.parametersBuilder . BU.parseParameters) jobParameters)

-- | Trigger a build for the given job with optional build parameters.
postBuild ::
  BasicAuthUser b => Maybe b   -- ^ The user to authenticate with.
  -> FollowOutputFlag        -- ^ Whether to follow log output or not.
  -> JobPath                   -- ^ The job to trigger a build against.
  -> Maybe JobParameters       -- ^ Optional set of job parameters to trigger with.
  -> Bartlett ()
postBuild user followFlag path parameters = do
  jenkins <- fromJust <$> asks jenkinsInstance
  resp <- liftIO $ execRequest Post reqOpts (BU.mkUrl jenkins path suffix) Nothing
  if followFlag
     then do
       buildNumber <- fromJust <$> getLatestBuildNumber user (BU.mkUrl jenkins path "/lastBuild/api/json")
       let inst = BU.mkUrl jenkins path $ "/" <> buildNumber <> "/logText/progressiveText"
       BL.requestLogs user inst followFlag "0"
     else
       liftIO $ BL.putStrLn . encodePretty . BU.toResponseStatus $
         resp ^. responseStatus
  where (suffix, buildOpts) = consBuildType parameters
        reqOpts = buildOpts & set auth (getBasicAuth <$> user)

getLatestBuildNumber ::
  BasicAuthUser b => Maybe b
  -> JenkinsInstance
  -> Bartlett (Maybe BL.ByteString)
getLatestBuildNumber user jenkins = do
  resp <- liftIO $ execRequest Get reqOpts jenkins Nothing
  return $ BU.toByteString <$> resp ^? responseBody . key (BU.toText "id") . _String
    where reqOpts = defaults & set auth (getBasicAuth <$> user)

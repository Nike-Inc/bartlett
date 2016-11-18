{-|
Module      : Build
Description : Methods for executing build requests against Jenkins
Copyright   : (c) Nike, Inc., 2016
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Methods for executing build requests against Jenkins.
-}
module Bartlett.Actions.Build (
  postBuild
) where

import Prelude hiding (putStrLn)

import Bartlett.Network (execRequest)
import Bartlett.Types
import qualified Bartlett.Util as BU

import Control.Lens ((?~), (^.), (&))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (Options, responseStatus, auth)


-- | Parses and determines type of job to trigger based on supplied parameters.
consBuildType :: Maybe JobParameters -> (BL.ByteString, Network.Wreq.Options)
consBuildType Nothing =
  ("/build", BU.optionsBuilder (BU.parametersBuilder [("", "")]))
consBuildType (Just jobParameters) =
  ("/buildWithParameters",
    (BU.optionsBuilder . BU.parametersBuilder . BU.parseParameters) jobParameters)

-- | Trigger a build for the given job with optional build parameters.
postBuild ::
  BasicAuthUser b => b   -- ^ The user to authenticate with.
  -> JenkinsInstance     -- ^ The Jenkins instance to make requests against.
  -> JobPath             -- ^ The job to trigger a build against.
  -> Maybe JobParameters -- ^ Optional set of job parameters to trigger with.
  -> IO ()
postBuild user base path parameters = do
  resp <- execRequest Post reqOpts reqUri Nothing
  BL.putStrLn . encodePretty . BU.toResponseStatus $
    resp ^. responseStatus
  where (suffix, buildOpts) = consBuildType parameters
        reqOpts = buildOpts & auth ?~ getBasicAuth user
        reqUri = BU.mkUrl base path suffix

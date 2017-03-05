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

import Bartlett.Network (execRequest)
import Bartlett.Types
import qualified Bartlett.Util as BU

import Control.Lens (set, (^.), (&))
import Control.Monad.Reader (asks, liftIO)
import Data.Maybe (fromJust)
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
  BasicAuthUser b => Maybe b   -- ^ The user to authenticate with.
  -> JobPath                   -- ^ The job to trigger a build against.
  -> Maybe JobParameters       -- ^ Optional set of job parameters to trigger with.
  -> Bartlett ()
postBuild user path parameters = do
  jenkins <- fromJust <$> asks jenkinsInstance
  resp <- liftIO $ execRequest Post reqOpts (BU.mkUrl jenkins path suffix) Nothing
  liftIO $ BL.putStrLn . encodePretty . BU.toResponseStatus $
    resp ^. responseStatus
  where (suffix, buildOpts) = consBuildType parameters
        reqOpts = buildOpts & set auth (getBasicAuth <$> user)

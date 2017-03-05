{-|
Module      : Log
Description : Methods for downloading job artifacts from Jenkins
Copyright   : (c) Nike, Inc., 2016-present
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Methods for downloading job artifacts from Jenkins.
-}
module Bartlett.Actions.Artifact (
  getArtifact
) where

import Bartlett.Network (execRequest)
import Bartlett.Types
import Bartlett.Util (mkUrl)

import Control.Lens (set, (^.), (&))
import Control.Monad.Reader (asks, liftIO)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (responseBody, defaults, auth)

-- | Download an artifact from the provided job.
getArtifact ::
  BasicAuthUser b => Maybe b -- ^ The user to authenticate with.
  -> JobPath                 -- ^ The job to get the artifact from.
  -> ArtifactId              -- ^ The artifact to get from the job.
  -> Bartlett ()
getArtifact user path artifactId = do
  jenkins <- fromJust <$> asks jenkinsInstance
  -- TODO make this a bit clearer
  resp <- liftIO $ execRequest Get reqOpts (mkUrl jenkins path $ "/lastSuccessfulBuild/artifact/" <> artifactId) Nothing
  liftIO $ BL.putStrLn $ resp ^. responseBody
    where reqOpts = defaults & set auth (getBasicAuth <$> user)

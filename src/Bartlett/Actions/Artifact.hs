{-|
Module      : Info
Description : Methods for executing informational requests against Jenkins
Copyright   : (c) Nike, Inc., 2016
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Methods for executing informational requests against Jenkins.
-}
module Bartlett.Actions.Artifact (
  getArtifact
) where

import Bartlett.Network (execRequest)
import Bartlett.Types
import Bartlett.Util (mkUrl)

import Control.Lens (set, (^.), (&))
import Data.Maybe (Maybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (responseBody, defaults, auth)

-- | Download an artifact from the provided job.
getArtifact ::
  BasicAuthUser b => Maybe b -- ^ The user to authenticate with.
  -> JenkinsInstance         -- ^ The Jenkins instance to authenticate against.
  -> JobPath                 -- ^ The job to get the artifact from.
  -> ArtifactId              -- ^ The artifact to get from the job.
  -> IO ()
getArtifact user base path artifactId = do
  resp <- execRequest Get reqOpts reqUri Nothing
  BL.putStrLn $ resp ^. responseBody
    where reqOpts = defaults & set auth (getBasicAuth <$> user)
          reqUri  = mkUrl base path $
                      BL.concat ["/lastSuccessfulBuild/artifact/", artifactId]

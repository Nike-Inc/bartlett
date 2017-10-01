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

import           Bartlett.Types
import           Bartlett.Util         (mkUrl, withJenkins)

import           Control.Lens          (set, (&), (^.))
import           Control.Monad.Reader  (ask, liftIO)
import qualified Data.ByteString.Char8 as BC
import           Data.Monoid           ((<>))
import           Network.Wreq          (auth, defaults, responseBody)

-- | Download an artifact from the given job.
getArtifact ::
  JobPath       -- ^ The job to get the artifact from.
  -> ArtifactId -- ^ The artifact to get from the job.
  -> Bartlett (Either BartlettError ())
getArtifact path artifactId = do
    options <- ask
    let uri = mkUrl (jenkinsInstance options) path $
                "/lastSuccessfulBuild/artifact/" <> artifactId
    let reqOpts = defaults & set auth (getBasicAuth <$> user options)
    let execRequest = requestExecutor options

    withJenkins uri $ do
      response <- execRequest Get reqOpts Nothing
      case response of
        (Left e) ->
          return . Left $ e
        Right resp -> do
          liftIO $ BC.putStrLn $ resp ^. responseBody
          return . Right $ ()

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

import           Bartlett.Network      (execRequest)
import           Bartlett.Types
import           Bartlett.Util         (mkUrl)

import           Control.Lens          (set, (&), (^.))
import           Control.Monad.Reader  (asks, liftIO)
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe            (fromJust)
import           Data.Monoid           ((<>))
import           Network.Wreq          (auth, defaults, responseBody)

-- | Download an artifact from the provided job.
getArtifact ::
  BasicAuthUser b => Maybe b -- ^ The user to authenticate with.
  -> JobPath                 -- ^ The job to get the artifact from.
  -> ArtifactId              -- ^ The artifact to get from the job.
  -> Bartlett (Either BartlettError ())
getArtifact user path artifactId =
  let reqOpts = defaults & set auth (getBasicAuth <$> user)
  in do
    jenkins <- fromJust <$> asks jenkinsInstance
    let uri = mkUrl jenkins path $ "/lastSuccessfulBuild/artifact/" <> artifactId
    -- TODO make this a bit clearer
    response <- execRequest Get reqOpts uri Nothing
    case response of
      (Left e) ->
        return . Left $ e
      Right resp -> do
        liftIO $ BC.putStrLn $ resp ^. responseBody
        return . Right $ ()

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

import           Bartlett.Types
import qualified Bartlett.Util            as Util

import           Control.Lens             (set, (&), (^.))
import           Control.Monad.Reader     (ask, liftIO)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as Lazy
import           Network.Wreq             (Options, auth, responseStatus)


-- | Parses and determines type of job to trigger based on supplied parameters.
consBuildType :: Maybe JobParameters -> (ByteString, Options)
consBuildType Nothing =
  ("/build", Util.optionsBuilder (Util.parametersBuilder [("", "")]))
consBuildType (Just jobParameters) =
  ("/buildWithParameters",
    Util.optionsBuilder . Util.parametersBuilder $ jobParameters)

-- | Trigger a build for the given job with optional build parameters.
postBuild ::
  JobPath                -- ^ The job to trigger a build against.
  -> Maybe JobParameters -- ^ Optional set of job parameters to trigger with.
  -> Bartlett (Either BartlettError ())
postBuild path parameters = do
  options <- ask
  let (suffix, buildOpts) = consBuildType parameters
  let uri = Util.mkUrl (jenkinsInstance options) path suffix
  let reqOpts = buildOpts & set auth (getBasicAuth <$> user options)
  let execRequest = requestExecutor options

  Util.withJenkins uri $ do
    response <- execRequest Post reqOpts Nothing
    case response of
      (Left e) ->
        return . Left $ e
      Right resp -> do
        liftIO $ BC.putStrLn
          . Lazy.toStrict
          . encodePretty
          . Util.toResponseStatus $ resp ^. responseStatus
        return . Right $ ()

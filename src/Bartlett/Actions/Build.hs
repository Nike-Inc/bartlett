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

import           Bartlett.Network         (execRequest)
import           Bartlett.Types           hiding (Options)
import qualified Bartlett.Util            as BU

import           Control.Lens             (set, (&), (^.))
import           Control.Monad.Reader     (asks, liftIO)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Maybe               (fromJust)
import           Network.Wreq             (Options, auth, responseStatus)


-- | Parses and determines type of job to trigger based on supplied parameters.
consBuildType :: Maybe JobParameters -> (ByteString, Options)
consBuildType Nothing =
  ("/build", BU.optionsBuilder (BU.parametersBuilder [("", "")]))
consBuildType (Just jobParameters) =
  ("/buildWithParameters",
    BU.optionsBuilder . BU.parametersBuilder $ jobParameters)

-- | Trigger a build for the given job with optional build parameters.
postBuild ::
  BasicAuthUser b => Maybe b   -- ^ The user to authenticate with.
  -> JobPath                   -- ^ The job to trigger a build against.
  -> Maybe JobParameters       -- ^ Optional set of job parameters to trigger with.
  -> Bartlett ()
postBuild user path parameters =
  let (suffix, buildOpts) = consBuildType parameters
      reqOpts = buildOpts & set auth (getBasicAuth <$> user)
  in do
  jenkins <- fromJust <$> asks jenkinsInstance
  resp <- liftIO $ execRequest Post reqOpts (BU.mkUrl jenkins path suffix) Nothing
  liftIO $ BC.putStrLn . Lazy.toStrict . encodePretty . BU.toResponseStatus $
    resp ^. responseStatus

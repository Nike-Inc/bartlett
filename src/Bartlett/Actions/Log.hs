{-|
Module      : Log
Description : Methods for printing or following log output
Copyright   : (c) Nike, Inc., 2016-present
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Methods for printing or following log output.
-}
module Bartlett.Actions.Log (
  getLogs
) where

import           Bartlett.Types
import           Bartlett.Util         (mkUrl, withJenkins)

import           Control.Concurrent    (threadDelay)
import           Control.Lens          (set, (&), (^.), (^?))
import           Control.Monad         (unless)
import           Control.Monad.Reader  (ask, asks, liftIO)
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe            (isJust)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as TE
import           Network.Wreq          (auth, defaults, param, responseBody,
                                        responseHeader)

-- | Internal helper to recursively get logs from the given Jenkins instance.
requestLogs ::
  FollowOutputFlag -- ^ Whether to recursively fetch logs or not.
  -> Text          -- ^ The current offset for log output
  -> Bartlett (Either BartlettError ())
requestLogs followFlag offset = do
  options <- ask
  let reqOpts = defaults & set auth (getBasicAuth <$> user options) . set (param "start") [offset]
  let execRequest = requestExecutor options

  resp <- execRequest Get reqOpts Nothing
  case resp of
    Left e ->
      return . Left $ e
    Right resp -> do
      unless (BC.null $ resp ^. responseBody) $
        liftIO . BC.putStr $ resp ^. responseBody
      if followFlag && isJust (resp ^? responseHeader "X-More-Data")
         then do
           liftIO $ threadDelay 1000000
           requestLogs followFlag $
            TE.decodeUtf8 (resp ^. responseHeader "X-Text-Size")
         else return . Right $ ()

-- | Fetch logs from the current Jenkins instance.
getLogs ::
  FollowOutputFlag        -- ^ Whether to follow log output or not.
  -> JobPath                 -- ^ The job to get logs for.
  -> BuildNumber             -- ^ The build number to get logs for.
  -> Bartlett (Either BartlettError ())
getLogs followFlag path buildNumber = do
  jenkins <- asks jenkinsInstance
  let inst = mkUrl jenkins path $ "/" <> buildNumber <> "/logText/progressiveText"
  withJenkins inst $
    requestLogs followFlag "0"

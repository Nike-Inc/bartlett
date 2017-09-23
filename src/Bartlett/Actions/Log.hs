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

import           Bartlett.Network           (execRequest)
import           Bartlett.Types
import           Bartlett.Util              (mkUrl)

import           Control.Concurrent         (threadDelay)
import           Control.Lens               (set, (&), (^.), (^?))
import           Control.Monad              (unless, when)
import           Control.Monad.Reader       (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                 (fromJust, isJust)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.Wreq               (auth, defaults, param,
                                             responseBody, responseHeader)

-- | Internal helper to recursively get logs from the given Jenkins instance.
requestLogs ::
  BasicAuthUser b => Maybe b -- ^ The user to authenticate with.
  -> JenkinsInstance         -- ^ The Jenkins instance to interact with.
  -> FollowOutputFlag        -- ^ Whether to recursively fetch logs or not.
  -> T.Text                  -- ^ The current offset for log output
  -> Bartlett ()
requestLogs user jenkins followFlag offset = do
  resp <- liftIO $ execRequest Get reqOpts jenkins Nothing
  unless (BL.null $ resp ^. responseBody) $
    liftIO $ BL.putStr $ resp ^. responseBody
  when (followFlag && isJust (resp ^? responseHeader "X-More-Data")) $ do
   liftIO $ threadDelay 1000000
   requestLogs user jenkins  followFlag $ TE.decodeUtf8 (resp ^. responseHeader "X-Text-Size")
    where reqOpts = defaults & set auth (getBasicAuth <$> user) . set (param "start") [offset]

-- | Fetch logs from the current Jenkins instance.
getLogs ::
  BasicAuthUser b => Maybe b -- ^ The user to authenticate with.
  -> FollowOutputFlag        -- ^ Whether to follow log output or not.
  -> JobPath                 -- ^ The job to get logs for.
  -> BuildNumber             -- ^ The build number to get logs for.
  -> Bartlett ()
getLogs user followFlag path buildNumber = do
  jenkins <- fromJust <$> asks jenkinsInstance
  let inst = mkUrl jenkins path $ "/" <> buildNumber <> "/logText/progressiveText"
  requestLogs user inst followFlag "0"
  return ()

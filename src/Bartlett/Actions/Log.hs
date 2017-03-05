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

import Bartlett.Network (execRequest)
import Bartlett.Types
import Bartlett.Util (mkUrl)

import Control.Lens (set, (^.), (&))
import Control.Monad.Reader (asks, liftIO)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (responseBody, defaults, auth, param)

getLogs ::
  BasicAuthUser b => Maybe b -- ^ The user to authenticate with.
  -> FollowOutputFlag        -- ^ Whether to follow log output or not.
  -> JobPath                 -- ^ The job to get logs for.
  -> BuildNumber             -- ^ The build number to get logs for.
  -> Bartlett ()
getLogs user followFlag path buildNumber = do
  jenkins <- fromJust <$> asks jenkinsInstance
  -- TODO make this a bit cleaner
  resp <- liftIO $ execRequest Get reqOpts (mkUrl jenkins path $ "/" <> buildNumber <> "/logText/progressiveText") Nothing
  liftIO $ BL.putStrLn $ resp ^. responseBody
  where reqOpts = defaults & set auth (getBasicAuth <$> user)
                    . set (param "start") ["0"]

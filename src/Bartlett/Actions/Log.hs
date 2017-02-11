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
import Data.Maybe (Maybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq (responseBody, defaults, auth, param)

getLogs ::
  BasicAuthUser b => Maybe b -- ^ The user to authenticate with.
  -> JenkinsInstance         -- ^ The Jenkins instance to authenticate against.
  -> FollowOutputFlag        -- ^ Whether to follow log output or not.
  -> JobPath                 -- ^ The job to get logs for.
  -> BuildNumber             -- ^ The build number to get logs for.
  -> IO ()
getLogs user base followFlag path buildNumber = do
  resp <- execRequest Get reqOpts reqUri Nothing
  BL.putStrLn $ resp ^. responseBody
    where reqOpts = defaults & set auth (getBasicAuth <$> user)
                      . set (param "start") ["0"]
          reqUri  = mkUrl base path $
            BL.concat ["/", buildNumber, "/logText/progressiveText"]

{-# LANGUAGE OverloadedStrings #-}
module ReplSugar where

import Control.Lens
import Bartlett.Types
import Bartlett.Util
import Bartlett.Configuration
import Bartlett.Network
import Data.Maybe
import Data.ByteString.Lazy.Char8
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S

-- | Docker Jenkins user.
usr :: User
usr = User "test" "password"

usrOpts :: W.Options
usrOpts =
  W.defaults & W.auth ?~ getBasicAuth usr


jenkins :: IO JenkinsInstance
jenkins = do
  c <- getConfiguration "default"
  j <- getJenkinsInstance c
  return $ fromJust j

getCSRF :: IO W.Options
getCSRF = do
  j <- jenkins
  S.withAPISession $ \session -> do
    foo <- consCSRFHeader $ requestCSRFToken session usrOpts j
    return $ W.defaults & foo

{-# LANGUAGE OverloadedStrings #-}
module ReplSugar where

import           Bartlett.Configuration
import           Bartlett.Network
import           Bartlett.Types
import           Bartlett.Util
import           Control.Lens
import           Data.ByteString.Lazy.Char8
import           Data.Maybe
import qualified Network.Wreq               as W
import qualified Network.Wreq.Session       as S

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
  j <- ReplSugar.jenkins
  S.withAPISession $ \session -> do
    csrfCrumb <- requestCSRFToken session usrOpts j
    case csrfCrumb of
      Right c ->
        return $ W.defaults & consCSRFHeader (crumbRequestField c, crumb c)
      Left _ ->
        return W.defaults

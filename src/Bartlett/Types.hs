{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Types
Description : Type declarations used throughout Bartlett
Copyright   : (c) Nike, Inc., 2016-present
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Types and type alises used throughout Bartlett.
-}
module Bartlett.Types (
  -- * Type Aliases
  JenkinsInstance,
  Username,
  Password,
  JobPath,
  JobParameters,
  Profile,
  ConfigPath,
  DeleteFlag,
  ArtifactId,
  FollowOutputFlag,
  BuildNumber,
  -- * User types
  BasicAuthUser(..),
  User(..),
  -- * Command-Line Types
  Command(..),
  CliOpts(..),
  -- * Network Types
  StatusResponse(..),
  RequestType(..),
  CrumbResponse(..),
  -- * Bartlett
  Env(..),
  Bartlett(..),
  BartlettError(..)
) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import qualified Network.Wreq           as Wreq
import           URI.ByteString         (Absolute, URIRef)

-- TODO use newtypes!! doesn't require boxing

type JenkinsInstance = URIRef Absolute
-- ^ Base URI for the desired Jenkins instance.
type Username        = ByteString
-- ^ Username to authenticate with against Jenkins.
type Password        = ByteString
-- ^ Password to authenticate with against Jenkins.
type JobPath         = ByteString
-- ^ Slash-delimited string representing the path to the job for the given
-- Jenkins instance.
type ArtifactId      = ByteString
-- ^ The artifact to download from the given job.
type JobParameters   = [(Text, Text)]
-- ^ Comma-separated list of key=value pairs to pass along to the triggered job.
type Profile         = Text
-- ^ The profile to use when authenticating against Jenkins.
type ConfigPath      = FilePath
-- ^ The path to the job configuration to upload.
type DeleteFlag      = Bool
-- ^ Should we delete the current item?
type FollowOutputFlag = Bool
-- ^ Should we follow log output for the current job?
type BuildNumber     = ByteString
-- ^ The build number for the given job.

-- | Defines methods for basic authentication
class BasicAuthUser a where
  -- ^ Retrieve the user and password information from the given object and
  -- return an 'Auth' object.
  getBasicAuth :: a -> Wreq.Auth

-- | Simple representation of a user
data User =
  User Username Password -- ^ Simple representation of a user.

-- | Basic auth implementation for 'User'
instance BasicAuthUser User where
  getBasicAuth (User usr pwd) = Wreq.basicAuth usr pwd

-- | Represents all available sub-commands for 'Bartlett'.
data Command =
  Info [JobPath]                                   -- ^ Retrieve information for the given job.
  | Build JobPath (Maybe JobParameters)            -- ^ Build the given job with the given options.
  | Config DeleteFlag [JobPath] (Maybe ConfigPath) -- ^ Retrieve and upload job configurations.
  | Artifact JobPath ArtifactId                    -- ^ Retrieve the given artifact from the given job.
  | Log FollowOutputFlag JobPath BuildNumber       -- ^ Print the log output for a given job.
  deriving (Show)

-- | Runtime options for 'Bartlett'
data Env = Env {
  user            :: Maybe User,
  jenkinsInstance :: JenkinsInstance,
  requestExecutor ::
    !(RequestType
      -> Wreq.Options
      -> Maybe ByteString
      -> Bartlett (Either BartlettError (Wreq.Response ByteString)))
}

-- | All available CLI options for 'Bartlett'.
data CliOpts = CliOpts {
  uname :: Maybe Username,
  jenkins :: Maybe JenkinsInstance,
  prof :: Maybe Profile,
  refreshCredentials :: Bool,
  cmd :: Command
}

-- | Enumeration of all known sources of failure in Bartlett.
data BartlettError =
  NetworkError
  -- ^ Something happened at the network layer that prevented an action from completing successfully.
  | ConfigurationError
  -- ^ There was a problem sourcing some required configuration item before completing successfully.
  | JenkinsError
  -- ^ Jenkins reported a non-2xx response for our attempted action.
  | CsrfProtectionNotEnabled
  -- ^ Non-fatal; there was an error attempting to get the CSRF crumb from Jenkins but it was not enabled.
  deriving (Eq, Show)

-- | The Bartlett Monad which encompases a few useful Monad Transformers.
newtype Bartlett a = Bartlett {
  runBartlett :: ReaderT Env IO a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

-- | Wrapper around Wreq's 'Status' type.
--
--   At this time 'Data.Aeson' does not support parsing 'ByteString', so accept a
--   'String' instead.
data StatusResponse = StatusResponse {
    statusCode :: Int,      -- ^ Status code for the response
    statusMessage :: Text -- ^ Message for the response
  }
  deriving (Eq, Generic, Show)
instance ToJSON StatusResponse
instance FromJSON StatusResponse

-- | Wrapper around the crumb response from Jenkins.
data CrumbResponse = CrumbResponse {
    crumbRequestField :: Text,
    crumb :: Text
  }
  deriving (Eq, Generic, Show)
instance ToJSON CrumbResponse
instance FromJSON CrumbResponse

-- | Incomplete sum type for network requests
data RequestType = Get | Post

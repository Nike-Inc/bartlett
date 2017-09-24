{-|
Module      : Parsers
Description : Parsers used to extract command line options at invocation
Copyright   : (c) Nike, Inc., 2016-present
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Parsers used to extract command line options at invocation.
-}
module Bartlett.Parsers where

import           Bartlett.Types

import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (pack, unpack)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Version              (showVersion)
import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import qualified Paths_bartlett
import           URI.ByteString            (Absolute, URIRef, parseURI,
                                            strictURIParserOptions)

-- | The current bartlett version
bartlettVersion :: String
bartlettVersion = "bartlett " <> showVersion Paths_bartlett.version

-- | Parse a command line option as a "ByteString".
readerByteString :: ReadM ByteString
readerByteString = do
  s <- readerAsk
  return $ pack s

readerText :: ReadM Text
readerText = do
  s <- readerAsk
  return $ T.pack s

readerJobParameters :: ReadM JobParameters
readerJobParameters = do
  s <- readerAsk
  if null s
     then return []
     else return $ fmap (trimEquals . T.breakOn "=") . T.splitOn "," . T.pack $ s
       where trimEquals (x,y) = (x, T.drop 1 y)

-- | Parse a command line option as a "URIRef"
readerUriRef :: ReadM (URIRef Absolute)
readerUriRef = do
  s <- readerAsk
  case parseURI strictURIParserOptions (pack s) of
    Left a ->
      readerAbort (ErrorMsg (show a))
    Right uri ->
      return uri

-- | Wrap parsers with doc strings and metadata.
withInfo :: Parser a -> ByteString -> ParserInfo a
withInfo opts desc = info (helper <*> parseVersion <*> opts)
  (fullDesc
  <> progDesc (unpack desc)
  <> header (bartlettVersion <> " - the Jenkins command-line tool to serve your needs.")
  <> footer "Copyright (c) Nike, Inc. 2016-present")

-- | Parse a version flag.
parseVersion :: Parser (a -> a)
parseVersion = infoOption bartlettVersion $
  long "version"
  <> help "Print the current version and exit."

-- | Parse a credentials flag.
parseRefreshCredentials :: Parser Bool
parseRefreshCredentials = switch $
  long "refresh-credentials"
  <> help "Force a refresh of the credentials cache for the current profile."

-- | Parse a 'Profile'.
parseProfile :: Parser Profile
parseProfile = option readerText $
  short 'p' <> long "profile" <> metavar "PROFILE_NAME" <>
  help "The profile to source values from"

-- | Parse a 'Username'.
parseUsername :: Parser Username
parseUsername = option readerByteString $
  short 'u' <> long "username" <> metavar "USERNAME" <>
  help "The user to authenticate with"

-- | Parse a Jenkins instance url.
parseJenkinsInstance :: Parser JenkinsInstance
parseJenkinsInstance = option readerUriRef $
  short 'j' <> long "jenkins" <> metavar "JENKINS_INSTANCE" <>
  help "The Jenkins instance to interact with"

-- | Parse a set of job parameters.
parseJobParameters :: Parser JobParameters
parseJobParameters = option readerJobParameters $
  short 'o' <> long "options" <> metavar "OPTIONS" <>
  help "Comma separated list of key=value pairs to pass to the job"

-- | Parse a path to the config file to update.
parseConfigFilePath :: Parser ConfigPath
parseConfigFilePath = option str $
  short 'f' <> long "filepath" <> metavar "CONFIG_FILE_PATH" <>
  help "If present, the path to the job configuration to upload for an existing job."

-- | Parse whether we should delete the given resource.
parseDeleteFlag :: Parser DeleteFlag
parseDeleteFlag = switch $
  short 'd' <> long "delete"
  <> help "Delete the given job path"

-- | Parse an Info sub-command.
parseInfo :: Parser Command
parseInfo = Info <$> some (argument readerByteString (metavar "JOB_PATHS..."))

-- | Parse a Build sub-command.
parseBuild :: Parser Command
parseBuild = Build
  <$> argument readerByteString (metavar "JOB_PATH")
  <*> optional parseJobParameters

-- | Parse a Config sub-command.
parseConfig :: Parser Command
parseConfig = Config
  <$> parseDeleteFlag
  <*> some (argument readerByteString (metavar "JOB_PATH..."))
  <*> optional parseConfigFilePath

-- | Parse an Artifact sub-command.
parseArtifact :: Parser Command
parseArtifact = Artifact
  <$> argument readerByteString (metavar "JOB_PATH")
  <*> argument readerByteString (metavar "ARTIFACT_ID")

-- | Parse whether we should follow log output for the given job.
parseFollowFlag :: Parser FollowOutputFlag
parseFollowFlag = switch $
  short 'f' <> long "follow"
  <> help "If present, follow log oputput for the current job"

-- | Parse a Log sub-command.
parseLog :: Parser Command
parseLog = Log
  <$> parseFollowFlag
  <*> argument readerByteString (metavar "JOB_PATH")
  <*> argument readerByteString (metavar "BUILD_NUMBER")

-- | Parse a Command.
parseCommand :: Parser Command
parseCommand = subparser $
  command "info" (parseInfo `withInfo` "Get information on the given job")
  <> command "build" (parseBuild `withInfo` "Trigger a build for the given job")
  <> command "config" (parseConfig `withInfo` "Manage XML configurations for jobs")
  <> command "artifact" (parseArtifact `withInfo` "Download artifacts from jobs")
  <> command "log" (parseLog `withInfo` "Print (or follow) log output for jobs")

-- | Combinator for all command line options.
parseOptions :: Parser Options
parseOptions = Options
  <$> optional parseUsername
  <*> optional parseJenkinsInstance
  <*> optional parseProfile
  <*> parseRefreshCredentials
  <*> parseCommand

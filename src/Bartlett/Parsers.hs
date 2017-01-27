{-|
Module      : Parsers
Description : Parsers used to extract command line options at invocation
Copyright   : (c) Nike, Inc., 2016
License     : BSD3
Maintainer  : fernando.freire@nike.com
Stability   : stable

Parsers used to extract command line options at invocation.
-}
module Bartlett.Parsers where

import Bartlett.Types

import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack, toStrict)
import URI.ByteString (URIRef, Absolute, parseURI, strictURIParserOptions)
import Options.Applicative
import Options.Applicative.Types (readerAsk)

-- | Parse a command line option as a "ByteString".
readerByteString :: ReadM ByteString
readerByteString = do
  s <- readerAsk
  return $ pack s

-- | Parse a command line option as a "URIRef"
readerUriRef :: ReadM (URIRef Absolute)
readerUriRef = do
  s <- readerAsk
  case parseURI strictURIParserOptions (toStrict $ pack s) of
    Left a ->
      readerAbort (ErrorMsg (show a))
    Right uri ->
      return uri

-- | Wrap parsers with doc strings and metadata.
withInfo :: Parser a -> ByteString -> ParserInfo a
withInfo opts desc = info (helper <*> opts)
  (fullDesc
  <> progDesc (unpack desc)
  <> header "bartlett - the Jenkins command-line tool to serve your needs."
  <> footer "Copyright (c) Nike, Inc. 2016")

-- | Parse a credentials flag.
parseRefreshCredentials :: Parser Bool
parseRefreshCredentials = switch $
  long "refresh-credentials"
  <> help "Force a refresh of the credentials cache for the current profile."

-- | Parse a 'Profile'.
parseProfile :: Parser Profile
parseProfile = option readerByteString $
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
parseJobParameters = option readerByteString $
  short 'o' <> long "options" <> metavar "OPTIONS" <>
  help "Comma separated list of key=value pairs to pass to the job"

-- | Parse a path to the config file to update.
parseConfigFilePath :: Parser ConfigPath
parseConfigFilePath = option str $
  short 'f' <> long "filepath" <> metavar "CONFIG_FILE_PATH" <>
  help "Path to the job configuration to upload"

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
  <*> argument readerByteString (metavar "JOB_PATH")
  <*> optional parseConfigFilePath

-- | Parse a Command.
parseCommand :: Parser Command
parseCommand = subparser $
  command "info" (parseInfo `withInfo` "Get information on the given job")
  <> command "build" (parseBuild `withInfo` "Trigger a build for the given job")
  <> command "config" (parseConfig `withInfo` "Manage XML configurations for jobs")

-- | Combinator for all command line options.
parseOptions :: Parser Options
parseOptions = Options
  <$> optional parseUsername
  <*> optional parseJenkinsInstance
  <*> optional parseProfile
  <*> parseRefreshCredentials
  <*> parseCommand

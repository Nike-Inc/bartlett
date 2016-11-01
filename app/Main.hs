module Main where

import Prelude hiding (putStr)

import Bartlett.Types
import Bartlett.Configuration
  (getConfiguration, getValueFromConfiguration)
import qualified Bartlett.Actions.Info as AI
import qualified Bartlett.Actions.Build as AB
import qualified Bartlett.Parsers as P

import Control.Exception
import Data.ByteString.Lazy.Char8 hiding (foldl)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Options.Applicative
import System.IO hiding (putStr, hPutStrLn, hPutStr)

-- | Wrapper determining if the given action should be echoed to stdout.
withEcho :: Bool -> IO a -> IO a
withEcho echo usrAction = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) usrAction

-- | Request a password from the user without emitting to stdout.
requestPassword :: IO ByteString
requestPassword = do
  -- Hack to get around shell buffering when using pipes.
  -- Basically, printing this message on stderr allows the user to know
  -- that their password is required before executing and passing its info
  -- to downstream commands in the shell pipe.
  hPutStr stderr "Enter password: "
  hFlush stdout
  s <- withEcho False getLine
  hPutChar stderr '\n'
  return $ pack s

-- | Given a list of possible values, choose the leftmost non-Nothing value.
chooseValue :: [Maybe a] -> Maybe a
chooseValue = getFirst . foldl mappend (First Nothing) . fmap First

-- | Execute the given command with the given username and jenkins instance.
executeCommand :: Command -> Username -> JenkinsInstance -> IO ()
executeCommand cmd usr jenkinsInstance = do
  pwd <- requestPassword
  case cmd of
    Info jobPaths ->
      AI.getInfo (User usr pwd) jenkinsInstance jobPaths
    Build jobPath jobParameters ->
      AB.postBuild (User usr pwd) jenkinsInstance jobPath jobParameters

-- | Execute the appropriate sub-command given parsed cli options.
run :: Options -> IO ()
run (Options username jenkinsInstance profile cmd) = do
  let profileName = fromMaybe "default" profile
  cfg <- getConfiguration

  -- TODO this is all very messy, surely there should be some way
  --      to simplify it.

  cfgJenkins <- getValueFromConfiguration profileName "jenkins_instance" cfg
  case chooseValue [jenkinsInstance, cfgJenkins] of
    Nothing ->
      hPutStrLn stderr "Could not determine the Jenkins instance to use."
    Just inst -> do

      cfgUser <- getValueFromConfiguration profileName "username" cfg
      case chooseValue [username, cfgUser] of
        Nothing ->
          hPutStrLn stderr "Could not determine username to use."
        Just usr ->
          executeCommand cmd usr inst

main :: IO ()
main = run =<< execParser (P.parseOptions `P.withInfo` "")

module Main where

import Prelude hiding (putStr)

import Bartlett.Types
import qualified Bartlett.Configuration as C
import qualified Bartlett.Actions.Info as AI
import qualified Bartlett.Actions.Build as AB
import qualified Bartlett.Actions.Config as AC
import qualified Bartlett.Parsers as P

import Control.Exception
import Data.ByteString.Lazy.Char8 hiding (foldl)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Options.Applicative
import System.IO hiding (putStr, hPutStrLn, hPutStr)
import qualified System.Keyring as SK

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

-- | Construct a namespaced service name for the OSX Keychain service.
keychainService :: Profile -> String
keychainService = unpack . mappend "bartlett."

-- | Given a username and profile retrieve the user's password.
--
-- Optionally store the user's password in the OSX Keychain.
selectPassword :: Bool -> Profile -> Username -> IO Password
selectPassword shouldStorePassword profile usr = do
  let service = SK.Service (keychainService profile)

  pwdFromKeyChain <-
    SK.getPassword service (SK.Username (unpack usr))

  case pwdFromKeyChain of
    Just (SK.Password pwd) ->
      return $ pack pwd
    Nothing -> do
      pwd <- requestPassword
      if shouldStorePassword
         then do
          SK.setPassword service (SK.Username (unpack usr)) (SK.Password (unpack pwd))
          return pwd
      else
        return pwd


-- | Execute the given command with the given username and jenkins instance.
executeCommand :: Command -> User -> JenkinsInstance -> IO ()
executeCommand cmd usr jenkinsInstance =
  case cmd of
    Info jobPaths ->
      AI.getInfo usr jenkinsInstance jobPaths
    Build jobPath jobParameters ->
      AB.postBuild usr jenkinsInstance jobPath jobParameters
    Config jobPath configFilePath ->
      case configFilePath of
        Just cp ->
          AC.updateConfig usr jenkinsInstance jobPath cp
        Nothing ->
          AC.getConfig usr jenkinsInstance jobPath

-- | Execute the appropriate sub-command given parsed cli options.
run :: Options -> IO ()
run (Options username jenkinsInstance profile cmd) = do
  let profileName = fromMaybe "default" profile
  cfg        <- C.getConfiguration profileName
  cfgJenkins <- C.getJenkinsInstance cfg
  cfgUser    <- C.getUsername cfg
  shouldStorePassword <- fromMaybe False <$> C.getStorePassword cfg

  case jenkinsInstance <|> cfgJenkins of
    Nothing ->
      hPutStrLn stderr "Could not determine the Jenkins instance to use."
    Just inst ->
      case username <|> cfgUser of
        Nothing ->
          hPutStrLn stderr "Could not determine username to use."
        Just usr -> do
          pwd <- selectPassword shouldStorePassword profileName usr
          executeCommand cmd (User usr pwd) inst

main :: IO ()
main = run =<< execParser (P.parseOptions `P.withInfo` "")

module Main where

import Prelude hiding (putStr)

import           Bartlett.Types
import           Bartlett.Parsers (parseOptions, withInfo)
import qualified Bartlett.Configuration  as C
import qualified Bartlett.Actions.Info   as AI
import qualified Bartlett.Actions.Build  as AB
import qualified Bartlett.Actions.Config as AC

import Control.Exception (bracket_)
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack, hPutStr)
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.Exit (die)
import System.IO (hFlush, stdout, stdin, stderr, hSetEcho, hGetEcho, hPutChar)
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
selectPassword :: Bool -> Bool -> Profile -> Username -> IO Password
selectPassword shouldStorePassword shouldRefreshCredentials profile usr = do
  let service = SK.Service (keychainService profile)

  -- Attempt to get the user's password from the Keychain service,
  -- unless we've been asked to refresh the credentials cache by the user
  -- via the --refresh-credentials flag
  pwdFromKeyChain <-
    if shouldRefreshCredentials
      then return Nothing
      else SK.getPassword service (SK.Username (unpack usr))

  case pwdFromKeyChain of
    Just (SK.Password pwd) ->
      return $ pack pwd
    Nothing -> do
      pwd <- requestPassword
      if shouldStorePassword
         then do
          let storeFn = if shouldRefreshCredentials
                           then SK.updatePassword
                           else SK.setPassword
          storeFn service (SK.Username (unpack usr)) (SK.Password (unpack pwd))
          return pwd
      else
        return pwd

-- | Bind an option, but if it is not provided exit the program.
bindOption :: Maybe a -> Maybe ByteString -> IO a
bindOption a failMessage =
  case a of
    Nothing ->
      die . unpack . fromMaybe "" $ failMessage
    Just a ->
      return a

-- | Execute the given command with the given username and jenkins instance.
executeCommand :: Command -> Maybe User -> JenkinsInstance -> IO ()
executeCommand cmd usr jenkinsInstance =
  case cmd of
    Info jobPaths ->
      AI.getInfo usr jenkinsInstance jobPaths
    Build jobPath jobParameters ->
      AB.postBuild usr jenkinsInstance jobPath jobParameters
    Config deleteFlag jobPath configFilePath ->
      if deleteFlag
         then AC.deleteConfig usr jenkinsInstance jobPath
         else
            case configFilePath of
              Just cp ->
                AC.updateConfig usr jenkinsInstance (head jobPath) cp
              Nothing ->
                AC.getConfig usr jenkinsInstance (head jobPath)

-- | Execute the appropriate sub-command given parsed cli options.
run :: Options -> IO ()
run (Options username jenkinsInstance profile refreshCredentials cmd) = do
  let profileName = fromMaybe "default" profile
  cfg <- C.getConfiguration profileName

  cfgJenkins <- C.getJenkinsInstance cfg
  jenkins    <- bindOption (jenkinsInstance <|> cfgJenkins)
                  (Just "Could not determine the Jenkins instance to use.")

  shouldStorePassword <- fromMaybe False <$> C.getStorePassword cfg
  cfgUser    <- C.getUsername cfg
  usr        <- userWithPassword (username <|> cfgUser)
                                 (selectPassword shouldStorePassword refreshCredentials profileName)

  executeCommand cmd usr jenkins

-- There is probably a better way to do this
userWithPassword :: Maybe Username -> (Username -> IO Password) -> IO (Maybe User)
userWithPassword Nothing _ = return Nothing
userWithPassword (Just username) getPwd = do
  password <- getPwd username
  return $ Just (User username password)

main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "")

module Main where

import Prelude hiding (putStr)

import           Bartlett.Types
import           Bartlett.Parsers (parseOptions, withInfo)
import qualified Bartlett.Configuration  as C
import qualified Bartlett.Actions.Info   as AI
import qualified Bartlett.Actions.Build  as AB
import qualified Bartlett.Actions.Config as AC
import qualified Bartlett.Actions.Artifact as AA
import qualified Bartlett.Actions.Log as AL

import Control.Exception (bracket_)
import Control.Monad.Reader (local, liftIO, ask, asks, runReaderT)
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

-- There is probably a better way to do this
userWithPassword :: Maybe Username -> (Username -> IO Password) -> IO (Maybe User)
userWithPassword Nothing _ = return Nothing
userWithPassword (Just username) getPwd = do
  password <- getPwd username
  return $ Just (User username password)

-- | Execute the appropriate sub-command given parsed cli options.
run :: Bartlett ()
run = do
  profile <- fromMaybe "default" <$> asks profile
  config  <- liftIO $ C.getConfiguration profile
  configJenkins <- liftIO $ C.getJenkinsInstance config
  usersJenkins <- asks jenkinsInstance
  jenkins <- liftIO $ bindOption (usersJenkins <|> configJenkins)
                (Just "Could not determine the Jenkins instance to use.")
  shouldStorePassword <- liftIO $ fromMaybe False <$> C.getStorePassword config
  configUser <- liftIO $ C.getUsername config
  userUser   <- asks username
  refreshCredentials <- asks refreshCredentials
  usr <- liftIO $ userWithPassword (userUser <|> configUser)
                                   (selectPassword shouldStorePassword refreshCredentials profile)
  command <- asks cmd
  options <- ask

  local (const $ options { jenkinsInstance=Just jenkins }) $
    case command of
      Info jobPaths ->
        AI.getInfo usr jobPaths
      Build jobPath jobParameters ->
        AB.postBuild usr jobPath jobParameters
      Artifact jobPath artifactId ->
        AA.getArtifact usr jobPath artifactId
      Log followFlag jobPath buildNumber ->
        AL.getLogs usr followFlag jobPath buildNumber
      Config deleteFlag jobPath configFilePath ->
        if deleteFlag
           then AC.deleteConfig usr jobPath
           else
              case configFilePath of
                Just cp ->
                  AC.updateConfig usr (head jobPath) cp
                Nothing ->
                  AC.getConfig usr (head jobPath)

main :: IO ()
main = execParser (parseOptions `withInfo` "") >>= \opts ->
  runReaderT (runBartlett run) opts

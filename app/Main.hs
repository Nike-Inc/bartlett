module Main where

import qualified Bartlett.Actions.Artifact as AA
import qualified Bartlett.Actions.Build    as AB
import qualified Bartlett.Actions.Config   as AC
import qualified Bartlett.Actions.Info     as AI
import qualified Bartlett.Actions.Log      as AL
import qualified Bartlett.Configuration    as C
import qualified Bartlett.Network          as Network
import           Bartlett.Parsers          (parseOptions, withInfo)
import           Bartlett.Types

import           Control.Applicative       ((<|>))
import           Control.Exception         (bracket_)
import           Control.Monad.Reader      (runReaderT)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Options.Applicative       (execParser)
import           Prelude                   hiding (putStr)
import           System.Exit               (die)
import           System.IO                 (hFlush, hGetEcho, hPutChar,
                                            hSetEcho, stderr, stdin, stdout)
import qualified System.Keyring            as SK

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
  BC.hPutStr stderr "Enter password: "
  hFlush stdout
  s <- withEcho False getLine
  hPutChar stderr '\n'
  return $ BC.pack s

-- | Construct a namespaced service name for the OSX Keychain service.
keychainService :: Profile -> String
keychainService = T.unpack . mappend "bartlett."

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
      else SK.getPassword service (SK.Username (BC.unpack usr))

  case pwdFromKeyChain of
    Just (SK.Password pwd) ->
      return $ BC.pack pwd
    Nothing -> do
      pwd <- requestPassword
      if shouldStorePassword
         then do
          let storeFn = if shouldRefreshCredentials
                           then SK.updatePassword
                           else SK.setPassword
          storeFn service (SK.Username (BC.unpack usr)) (SK.Password (BC.unpack pwd))
          return pwd
      else
        return pwd

-- | Bind an option, but if it is not provided exit the program.
bindOption :: Maybe a -> Text -> IO a
bindOption a failMessage =
  case a of
    Nothing ->
      die . T.unpack $ failMessage
    Just a ->
      return a

-- There is probably a better way to do this
userWithPassword :: Maybe Username -> (Username -> IO Password) -> IO (Maybe User)
userWithPassword Nothing _ = return Nothing
userWithPassword (Just username) getPwd = do
  password <- getPwd username
  return $ Just (User username password)

-- | Execute the appropriate sub-command given parsed cli options.
run :: Command -> Bartlett (Either BartlettError ())
run cmd =
  case cmd of
    Info jobPaths ->
      AI.getInfo jobPaths
    Build jobPath jobParameters ->
      AB.postBuild jobPath jobParameters
    Artifact jobPath artifactId ->
      AA.getArtifact jobPath artifactId
    Log followFlag jobPath buildNumber ->
      AL.getLogs followFlag jobPath buildNumber
    Config deleteFlag jobPath configFilePath ->
      if deleteFlag
         then AC.deleteConfig jobPath
         else
            case configFilePath of
              Just cp ->
                AC.updateConfig (head jobPath) cp
              Nothing ->
                AC.getConfig (head jobPath)

main :: IO ()
main = execParser (parseOptions `withInfo` "") >>= \cliOpts ->
  let profile = fromMaybe "default" (prof cliOpts)
      shouldRefreshCreds = refreshCredentials cliOpts
  in do
    userConfig <- C.getConfiguration profile
    profileJenkins <- C.getJenkinsInstance userConfig
    jenkins <- bindOption (jenkins cliOpts <|> profileJenkins) "Could not determine Jenkins instance to use."
    shouldStorePassword <- fromMaybe False <$> C.getStorePassword userConfig

    userConfigName <- C.getUsername userConfig
    usr <- userWithPassword (uname cliOpts <|> userConfigName)
                            (selectPassword shouldStorePassword shouldRefreshCreds profile)

    let env = Env usr jenkins Network.execRequest

    -- TODO figure out if more needs to be done here for printing errors
    returnValue <- runReaderT (runBartlett . run . cmd $ cliOpts) env
    case returnValue of
      Left e ->
        print e
      Right _ ->
        return ()

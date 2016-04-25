{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-orphans #-}

module Config
    ( Config(Config), SmtpConfig(SmtpConfig)
    , GetConfig(..), MonadReaderConfig
    , WarnMissing(DontWarnMissing, WarnMissing, CrashMissing)
    , PersistenceImpl(..)
    , aulaRoot
    , dbPath
    , defaultRecipient
    , exposedUrl
    , getSamplesPath
    , htmlStatic
    , listenerInterface
    , listenerPort
    , persistConfig
    , persistenceImpl
    , readConfig
    , releaseVersion
    , senderEmail
    , senderName
    , sendmailArgs
    , sendmailPath
    , setCurrentDirectoryToAulaRoot
    , smtpConfig
    , snapshotIntervalMinutes
    , logLevel
    )
where

import Control.Exception (throwIO, ErrorCall(ErrorCall))
import Control.Lens
import Control.Monad.Reader (MonadReader)
import Data.Functor.Infix ((<$$>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (SBS, cs)
import Data.Version (showVersion)
import Data.Yaml
import GHC.Generics
import System.Directory
import System.Environment
import System.FilePath ((</>))
import Thentos.Frontend.CSRF (GetCsrfSecret(..), CsrfSecret(..))

import Logger

import qualified Paths_aula as Paths
-- (if you are running ghci and Paths_aula is not available, try `-idist/build/autogen`.)


-- | FIXME: move this instance upstream and remove -fno-warn-orphans for this module.
instance ToJSON CsrfSecret where
  toJSON (CsrfSecret s) = String $ cs s

-- | FIXME: move this instance upstream and remove -fno-warn-orphans for this module.
instance FromJSON CsrfSecret where
  parseJSON o = CsrfSecret . (cs :: String -> SBS) <$> parseJSON o

data PersistenceImpl = AcidStateInMem | AcidStateOnDisk
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Enum, Bounded)

data SmtpConfig = SmtpConfig
    { _senderName       :: String
    , _senderEmail      :: String
    , _defaultRecipient :: Maybe String  -- (e.g. for use in demo data.)
    , _sendmailPath     :: String
    , _sendmailArgs     :: [String]
   -- ^ Not using 'ST' here since Network.Mail.Mime wants 'String' anyway.
    }
  deriving (Show, Generic, ToJSON, FromJSON) -- FIXME,JSON: customize the field names

makeLenses ''SmtpConfig

data PersistConfig = PersistConfig
    { _dbPath                  :: String
    , _persistenceImpl         :: PersistenceImpl
    , _snapshotIntervalMinutes :: Int
    }
  deriving (Show, Generic, ToJSON, FromJSON) -- FIXME,JSON: customize the field names

makeLenses ''PersistConfig

data Config = Config
    { _exposedUrl        :: String  -- e.g. https://aula-stage.liqd.net
    , _listenerInterface :: String
    , _listenerPort      :: Int
    , _htmlStatic        :: FilePath
    , _cfgCsrfSecret     :: CsrfSecret
    , _logLevel          :: LogLevel
    , _persistConfig     :: PersistConfig
    , _smtpConfig        :: SmtpConfig
    }
  deriving (Show, Generic, ToJSON, FromJSON) -- FIXME,JSON: customize the field names

makeLenses ''Config

class GetConfig r where
    getConfig :: Getter r Config

    viewConfig :: MonadReader r m => m Config
    viewConfig = view getConfig

type MonadReaderConfig r m = (MonadReader r m, GetConfig r)

instance GetConfig Config where
    getConfig = id

instance GetCsrfSecret Config where
    csrfSecret = pre cfgCsrfSecret

defaultSmtpConfig :: SmtpConfig
defaultSmtpConfig = SmtpConfig
    { _senderName       = "Aula Notifications"
    , _senderEmail      = "aula@example.com"
    , _defaultRecipient = Nothing
    , _sendmailPath     = "/usr/sbin/sendmail"
    , _sendmailArgs     = ["-t"]
    }

defaultPersistConfig :: PersistConfig
defaultPersistConfig = PersistConfig
    { _dbPath                  = "./state/AulaData"
    , _persistenceImpl         = AcidStateInMem
    , _snapshotIntervalMinutes = 47
    }

defaultConfig :: Config
defaultConfig = Config
    { _exposedUrl        = "http://localhost:8080"
    , _listenerInterface = "0.0.0.0"
    , _listenerPort      = 8080
    , _htmlStatic        = "./static"
    -- FIXME: BEWARE, this "secret" is hardcoded and public.
    , _cfgCsrfSecret     = CsrfSecret "1daf3741e8a9ae1b39fd7e9cc7bab44ee31b6c3119ab5c3b05ac33cbb543289c"
    , _logLevel          = DEBUG
    , _persistConfig     = defaultPersistConfig
    , _smtpConfig        = defaultSmtpConfig
    }

data WarnMissing = DontWarnMissing | WarnMissing | CrashMissing
  deriving (Eq, Show)

readConfig :: SendLogMsg -> WarnMissing -> IO Config
readConfig logger warnMissing = configFilePath >>= maybe (errr msgAulaPathNotSet >> dflt) decodeFileDflt
  where
    dflt :: IO Config
    dflt = pure defaultConfig

    decodeFileDflt :: FilePath -> IO Config
    decodeFileDflt fp = decodeFileEither fp >>= either (\emsg -> errr (msgParseError emsg) >> dflt) pure

    msgAulaPathNotSet :: [String]
    msgAulaPathNotSet =
        [ "no config file found: $AULA_ROOT_PATH not set."
        , "to fix this, write the following lines to $AULA_ROOT_PATH/aula.yaml:"
        ]

    msgParseError :: Show a => a -> [String]
    msgParseError emsg =
        [ "could not read config file:"
        , show emsg
        , "to fix this, write the following lines to $AULA_ROOT_PATH/aula.yaml:"
        ]

    errr :: [String] -> IO ()
    errr msgH = case warnMissing of
        DontWarnMissing -> pure ()
        WarnMissing     -> logger . LogEntry ERROR $ cs msgs
        CrashMissing    -> throwIO . ErrorCall $ msgs
      where
        msgs = unlines $ [""] <> msgH <> ["", cs $ encode defaultConfig]

configFilePath :: IO (Maybe FilePath)
configFilePath = (</> "aula.yaml") <$$> aulaRoot

aulaRoot :: IO (Maybe FilePath)
aulaRoot = lookup "AULA_ROOT_PATH" <$> getEnvironment

setCurrentDirectoryToAulaRoot :: IO ()
setCurrentDirectoryToAulaRoot = aulaRoot >>= maybe (pure ()) setCurrentDirectory

getSamplesPath :: IO FilePath
getSamplesPath = fromMaybe (error msg) . lookup var <$> getEnvironment
  where
    var = "AULA_SAMPLES"
    msg = "please set $" <> var <> " to a path (will be created if n/a)"

-- * release version

releaseVersion :: String
releaseVersion = "[v" <> showVersion Paths.version <> "]"

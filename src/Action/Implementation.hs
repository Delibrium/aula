{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- | The 'Action.Implementation' module contains a monad stack implmententation of the 'Action'
-- interface.
module Action.Implementation
    ( Action
    , mkRunAction
    )
where

import Control.Exception (SomeException(SomeException), catch)
import Control.Lens
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Prelude
import Servant
import Servant.Missing
import Thentos.Action (freshSessionToken)
import Thentos.Prelude (DCLabel, MonadLIO(..), MonadRandom(..), evalLIO, LIOState(..), dcBottom)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Acid as Acid

import Action
import Persistent


-- * concrete monad type

-- | The actions a user can perform.
newtype Action a = MkAction { unAction :: ExceptT ActionExcept (RWST ActionEnv () UserState IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError ActionExcept
             , MonadReader ActionEnv
             , MonadState UserState
             , MonadIO
             )

instance ActionError Action

instance ActionLog Action where
    logEvent = liftIO . print

-- | FIXME: test this (particularly strictness and exceptions)
instance ActionPersist Action where
    aquery (AQuery q) = do
        rp <- view persistNat
        v  <- liftIO . runExceptT $ Acid.query (rp ^. rpState) <$> q
        either (throwError . ActionExcept . unPersistExcept) pure v

    aupdate (AUpdate u) = do
        rp <- view persistNat
        v  <- liftIO . runExceptT $ Acid.update (rp ^. rpState) u
        either (throwError . ActionExcept . unPersistExcept) pure v

instance MonadLIO DCLabel Action where
    liftLIO = liftIO . (`evalLIO` LIOState dcBottom dcBottom)

instance MonadRandom Action where
    getRandomBytes = liftIO . getRandomBytes

instance ActionUserHandler Action where
    login uid = do
        usUserId .= Just uid
        sessionToken <- freshSessionToken
        usSessionToken .= Just sessionToken

    userState = use

    logout = put userLoggedOut

instance ActionTempCsvFiles Action where
    popTempCsvFile = liftIO . (`catch` exceptToLeft) . fmap decodeCsv . LBS.readFile
      where
        exceptToLeft (SomeException e) = return . Left . show $ e

    cleanupTempCsvFiles = liftIO . releaseFormTempFiles

-- | Creates a natural transformation from Action to the servant handler monad.
-- See Frontend.runFrontend for the persistency of @UserState@.
mkRunAction :: ActionEnv -> Action :~> ExceptT ServantErr IO
mkRunAction env = Nat run
  where
    run = withExceptT unActionExcept . ExceptT . fmap (view _1) . runRWSTflip env userLoggedOut
        . runExceptT . unAction . (checkCurrentUser >>)
    runRWSTflip r s comp = runRWST comp r s

    checkCurrentUser = do
        isValid <- userState $ to validUserState
        unless isValid $ do
            logout
            throwError500 "Invalid internal user session state"

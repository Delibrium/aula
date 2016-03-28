{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC #-}

module AulaTests.Stories where

import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.List
import Data.String
import Data.String.Conversions
import Data.Typeable (Typeable, typeOf)
import Lucid (Html, ToHtml, toHtml, renderText)
import Servant (unNat)
import Servant.Server.Internal.ServantErr
import Test.Hspec
import Test.QuickCheck
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT

import Arbitrary
import Types

import AulaTests.Stories.DSL
import AulaTests.Stories.Interpreter.Action


spec :: Spec
spec = describe "stories" $ it "works" $ do
    liftIO $ do
        print "---------------------------"
        run program
        print "---------------------------"
    True `shouldBe` True


program :: Behavior ()
program = do
    login "admin"
    selectIdeaSpace "school"
    createIdea (ProtoIdea "title" (Markdown "desc") CatRule (IdeaLocationSpace SchoolSpace))
    logout

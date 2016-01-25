{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains an example application created
-- using the connections library.
--------------------------------------------------------------------
module Main where

import Control.Applicative (Applicative)
import Control.Monad ((>>=), Monad, return)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Either (either)
import Data.Function((.), ($), const)
import Data.Functor (Functor)
import Data.List ((++))
import Data.Proxy (Proxy(Proxy))
import System.IO (IO, print)
import Text.Show (Show, show)

import Network.Connections (runClient)
import Network.Connections.Instances.TCP ()
import Network.Connections.Types.TCP (TCP, mkTCPSettings)

data TCPConnectionError
  = ConnectionRefused
  | DataSendError
  | DataReceiveError
  | CloseError

instance Show TCPConnectionError where
    show = \case
        ConnectionRefused
            -> withPrefix "The connection to the remote party was refused."
        DataSendError -> withPrefix "Could not send data to socket."
        DataReceiveError -> withPrefix "Could not receive data from socket."
        CloseError -> withPrefix "Could not close the socket."
      where
        withPrefix = ("TCPConnectionError: " ++)

newtype TCPConnectionT m a =
    TCPConnectionT { runTCP :: ExceptT TCPConnectionError m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadError TCPConnectionError
    )

main :: IO ()
main = eval client >>= either print return
  where
    eval = runExceptT . runTCP

client :: (MonadIO m, MonadCatch m) => TCPConnectionT m ()
client = runClient
    (Proxy :: Proxy TCP)
    settings
    onConnectHandler
    onCloseHandler
    sampleApp
  where
    settings = mkTCPSettings "127.0.0.1" 4444
    onConnectHandler = const $ throwError ConnectionRefused
    onCloseHandler = const $ throwError CloseError
    sampleApp _ = return ()

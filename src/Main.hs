{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
import Control.Concurrent (threadDelay)
import Control.Exception.Errno
    ( BadFileDescriptorError
    , BrokenPipeError
    , CallInteruptedError
    , ConnectionRefusedError
    , HostUnreachableError
    , InputOutputError
    )
import Control.Lens ((^.))
import Control.Monad ((>>=), Monad, return, void)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.Function((.), ($), const)
import Data.Functor (Functor)
import Data.List ((++))
import Data.Proxy (Proxy(Proxy))
import Network.Socket (Socket)
import System.IO (IO, print)
import Text.Show (Show, show)

import Network.Connections (runClient)
import Network.Connections.Instances.TCP ()
import Network.Connections.Internal.Types.Exception (catch', handle')
import Network.Connections.Types.TCP (TCP, mkTCPSettings)
import Network.Connections.Types.ConnectionData (recv, send)

data TCPConnectionError
    = CloseError
    | ConnectionRefused
    | HostUnreachable
    | DataSendError
    | DataReceiveError

instance Show TCPConnectionError where
    show = \case
        ConnectionRefused
            -> withPrefix "The connection to the remote party was refused."
        DataSendError
            -> withPrefix "Could not send data to the socket."
        DataReceiveError
            -> withPrefix "Could not receive data from the socket."
        CloseError
            -> withPrefix "Could not close the socket."
        HostUnreachable
            -> withPrefix "Destination host could not be reached."
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

client :: (MonadIO m, MonadCatch m)
       => TCPConnectionT m ()
client = runClient
    (Proxy :: Proxy TCP)
    settings
    onConnectHandler
    onCloseHandler
    sampleApp
  where
    settings = mkTCPSettings "10.0.0.139" 4444

    onConnectHandler computation = computation
        `catch'` onConnectionRefused
        `catch'` onHostUnreachable

    onConnectionRefused
        :: (Monad m)
        => ConnectionRefusedError
        -> TCPConnectionT m Socket
    onConnectionRefused = const $ throwError ConnectionRefused

    onHostUnreachable
        :: (Monad m)
        => HostUnreachableError
        -> TCPConnectionT m Socket
    onHostUnreachable = const $ throwError HostUnreachable

    onCloseHandler computation = computation
        `catch'` onIOError
        `catch'` onBadFileDescriptor
        `catch'` onCallInterrupted

    onIOError
        :: (Monad m)
        => InputOutputError
        -> TCPConnectionT m ()
    onIOError = const $ throwError CloseError

    onBadFileDescriptor
        :: (Monad m)
        => BadFileDescriptorError
        -> TCPConnectionT m ()
    onBadFileDescriptor = const $ throwError CloseError

    onCallInterrupted
        :: (Monad m)
        => CallInteruptedError
        -> TCPConnectionT m ()
    onCallInterrupted = const $ throwError CloseError

    sampleApp cd = do
        liftIO (threadDelay tenSeconds)
        void $ safeSend "Ahoj Svete"
        void safeRecv
      where
        safeSend = handle' onSendError . (cd ^. send)
        safeRecv = (cd ^. recv) `catch'` onRecvError

        onSendError
            :: (Monad m)
            => BrokenPipeError
            -> TCPConnectionT m ()
        onSendError = const $ throwError DataSendError

        onRecvError
            :: (Monad m)
            => BrokenPipeError
            -> TCPConnectionT m ByteString
        onRecvError = const $ throwError DataReceiveError

        tenSeconds = 10000000

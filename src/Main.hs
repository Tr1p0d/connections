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
import Control.Exception.Errno (ConnectionRefusedError, HostUnreachableError)
import Control.Lens ((^.), (&))
import Control.Monad ((>>=), Monad, return, void)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.TaggedException as E (Throws)
import Data.Either (either)
import Data.Function((.), ($){-, const-})
import Data.Functor (Functor)
import Data.List ((++))
import Data.Proxy (Proxy(Proxy))
import Network.Socket (Socket)
import System.IO (IO, print)
import Text.Show (Show, show)

import Network.Connections (runClient)
import Network.Connections.Instances.TCP ()
import Network.Connections.Internal.Types.Exception
    ( EvalThrows
    , FromThrows
    , catch'
    )
import Network.Connections.Types.TCP (TCP, mkTCPSettings)
import Network.Connections.Types.ConnectionData (recv, send)

import Prelude (undefined)

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

client :: TCPConnectionT IO ()
client = runClient
    (Proxy :: Proxy TCP)
    settings
    onConnectHandler
    onCloseHandler
    sampleApp
  where
    settings = mkTCPSettings "127.0.0.1" 4444

    sampleApp cd = do
        liftIO (threadDelay tenSeconds)
        void $ safeSend "Ahoj Svete"
        void safeRecv
      where
        safeSend = undefined
        safeRecv = undefined
        tenSeconds = 10000000

onConnectHandler
    ::
    ( MonadError TCPConnectionError (TCPConnectionT IO)
    , FromThrows (E.Throws
        '[]
        (TCPConnectionT IO) Socket)
    )
    => E.Throws [ConnectionRefusedError, HostUnreachableError]
        (TCPConnectionT IO) Socket
    -> EvalThrows (E.Throws '[] (TCPConnectionT IO) Socket)
onConnectHandler computation = computation
    `catch'` onConnectionRefused
    `catch'` onHostUnreachable

--    onConnectionRefused
--        ::
--        ( Monad m
--        , MonadError TCPConnectionError (TCPConnectionT m)
--        , FromThrows (E.Throws
--            '[HostUnreachableError] (TCPConnectionT m) Socket)
--        )
--        => ConnectionRefusedError
--        -> EvalThrows (E.Throws
--            '[HostUnreachableError] (TCPConnectionT m) Socket)
onConnectionRefused e = throwError ConnectionRefused
--       onHostUnreachable
--        ::
--        ( Monad m
--        , MonadError TCPConnectionError (TCPConnectionT m)
--        , FromThrows (E.Throws
--            '[] (TCPConnectionT m) Socket)
--        )
--        => HostUnreachableError
--        -> EvalThrows (E.Throws '[] (TCPConnectionT m) Socket)
onHostUnreachable e = throwError HostUnreachable

onCloseHandler = undefined

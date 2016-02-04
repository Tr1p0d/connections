{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
-- |
-- Module      :  TCP Connection type class instance
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains an implementation of the Connection
-- instance for the TCP data type.
--------------------------------------------------------------------
module Network.Connections.Instances.TCP where

import Control.Applicative ((<$>))
import Control.Exception.Errno
    ( BadFileDescriptorError
    , BrokenPipeError
    , CallInteruptedError
    , ConnectionRefusedError
    , HostUnreachableError
    , InputOutputError
    )
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.TaggedException.Internal.Throws as E
    (Throws(Throws))
import Data.Function ((.), ($), flip)
import Data.Tuple (fst)
import qualified Network.Socket as Socket
    ( Family(AF_INET)
    , Socket
    , )
import qualified Network.Socket.ByteString as Socket (send, recv)

import Network.Connections.Class.Connection
  ( Connection
  , ConnectionSettings
  , CloseConnectionError
  , ConnectionAccessor
  , EstablishConnectionError
  , RecvError
  , SendError
  , closeConnection
  , establishConnection
  , recvData
  , sendData
  )
import Network.Connections.Types.TCP (TCP, TCPSettings, host, port)
import Network.Connections.Internal.Types.Exception ()
import Network.Connections.Internal.Networking.TCP
    ( getTCPSocketAddress
    , closeSocket
    )
instance Connection TCP where
    type ConnectionAccessor TCP = Socket.Socket
    type ConnectionSettings TCP = TCPSettings

    type EstablishConnectionError TCP =
        [ConnectionRefusedError, HostUnreachableError]
    establishConnection _p s =
        E.Throws $ liftIO $ fst
        <$> getTCPSocketAddress (s ^. host) (s ^. port) Socket.AF_INET

    type CloseConnectionError TCP =
        [ CallInteruptedError
        , InputOutputError
        , BadFileDescriptorError
        ]
    closeConnection _ = E.Throws . liftIO . closeSocket

    type SendError TCP = '[BrokenPipeError]
    sendData _ = ((E.Throws . liftIO . void) .) . Socket.send

    type RecvError TCP = '[BrokenPipeError]
    recvData _ = E.Throws . liftIO . flip Socket.recv 4096

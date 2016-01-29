{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

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
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.TaggedException as E (liftT)
import Data.Function ((.), ($), flip)
import Data.Tuple (fst)
import qualified Network.Socket as Socket
    ( Family(AF_INET)
    , Socket
    , close
    , )
import qualified Network.Socket.ByteString as Socket (send, recv)
import System.IO.Error (IOError)

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
import Network.Connections.Internal.Types.Exception (ConnectionRefusedException)
import Network.Connections.Internal.Networking.TCP (getTCPSocketAddress)

instance Connection TCP where
    type ConnectionAccessor TCP = Socket.Socket
    type ConnectionSettings TCP = TCPSettings

    type EstablishConnectionError TCP = IOError-- ConnectionRefusedException
    establishConnection _p s =
        E.liftT $ liftIO $ fst
        <$> getTCPSocketAddress (s ^. host) (s ^. port) Socket.AF_INET

    type CloseConnectionError TCP = IOError
    closeConnection _ = E.liftT . liftIO . Socket.close

    type SendError TCP = IOError
    sendData _ = ((E.liftT . liftIO . void) .) . Socket.send

    type RecvError TCP = IOError
    recvData _ = E.liftT . liftIO . flip Socket.recv 4096

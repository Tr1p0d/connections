{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
-- |
-- Module      :  TCP Transport type class instance
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains an implementation of the Transport
-- instance for the TCP data type.
--------------------------------------------------------------------
module Network.Connections.Instances.TCP.Transport where

import Control.Exception.Errno (BrokenPipeError)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.TaggedException.Internal.Throws as E
    (Throws(Throws))
import Data.Function ((.), flip)
import qualified Network.Socket as Socket (Socket)

import Network.Connections.Internal.Networking.TCP (recv, send)
import Network.Connections.Types.TCP (TCP, TCPSettings)
import Network.Connections.Class.Transport
    ( ClientSettings
    , ConnectionAccessor
    , RecvError
    , SendError
    , ServerSettings
    , Transport
    , sendData
    , recvData
    )

instance Transport TCP where
    type ConnectionAccessor TCP = Socket.Socket
    type ServerSettings TCP = TCPSettings
    type ClientSettings TCP = TCPSettings

    type SendError TCP = '[BrokenPipeError]
    sendData _ = ((E.Throws . liftIO . void) .) . send

    type RecvError TCP = '[BrokenPipeError]
    recvData _ = E.Throws . liftIO . flip recv 4096

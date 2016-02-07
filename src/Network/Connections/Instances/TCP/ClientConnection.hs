{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
-- |
-- Module      :  TCP ClientConnection type class instance
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains an implementation of the ClientConnection
-- instance for the TCP data type.
--------------------------------------------------------------------
module Network.Connections.Instances.TCP.ClientConnection where

import Control.Applicative ((<$>))
import Control.Exception.Errno
    ( BadFileDescriptorError
    , CallInteruptedError
    , ConnectionRefusedError
    , HostUnreachableError
    , InputOutputError
    )
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.TaggedException.Internal.Throws as E
    (Throws(Throws))
import Data.Function ((.), ($))
import Data.Tuple (fst)
import qualified Network.Socket as Socket (Family(AF_INET))

import Network.Connections.Class.Connection
    ( ClientConnection
    , CloseConnectionError
    , EstablishConnectionError
    , closeConnection
    , establishConnection
    )
import Network.Connections.Types.TCP (TCP, host, port)
import Network.Connections.Instances.TCP.Transport ()
import Network.Connections.Internal.Types.Exception ()
import Network.Connections.Internal.Networking.TCP
    ( getTCPSocketAddress
    , closeSocket
    )
instance ClientConnection TCP where
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

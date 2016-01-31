{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------
-- |
-- Module      :  IOException wrappers
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides wrappers for IOException to make
-- Thows signatures more readable.
--------------------------------------------------------------------
module Network.Connections.Internal.Networking.TCP where

--import Control.Applicative ((<$>))
import Control.Exception ({-mapException, -}throw, handle)
import Control.Exception.Errno
    ( ConnectionRefused(ConnectionRefused)
    , ConnectionRefusedError
    , HostUnreachable(HostUnreachable)
    , HostUnreachableError
    )
import Data.ByteString (ByteString)
import Data.Int (Int)
import Data.Function ((.))
import Data.Function (($))
import Data.Maybe (maybe)
import Data.Streaming.Network (getSocketFamilyTCP)
--import GHC.IO.Exception (IOErrorType(NoSuchThing))
import qualified Network.Socket as Socket
    ( Family
    , SockAddr
    , Socket
    )
import System.IO (IO)
import System.IO.Error (IOError{-, ioeGetErrorType-})

import Network.Connections.Internal.Types.Exception (type (:^:)(E1, E2))

import Prelude (undefined)
--import Debug.Trace (traceShow)
import GHC.IO.Exception (ioe_errno)

getTCPSocketAddress
    :: ByteString
    -> Int
    -> Socket.Family
    -> IO (Socket.Socket, Socket.SockAddr)
--getTCPSocketAddress = (((mapException mapper <$>) .) .) getSocketFamilyTCP
getTCPSocketAddress = ((((throw . mapper) `handle`) .) .) . getSocketFamilyTCP
  where
    mapper :: IOError -> HostUnreachableError :^: ConnectionRefusedError
    mapper e = maybe (handleNoErrno e) handleByErrno (ioe_errno e)
      where
        handleNoErrno = undefined
        handleByErrno = \case
            111 -> E2 $ ConnectionRefused e
            113 -> E1 $ HostUnreachable e
            _ -> undefined
    -- traceShow no $ ConnectionRefused e

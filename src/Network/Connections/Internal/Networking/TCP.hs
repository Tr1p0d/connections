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
    ( BadFileDescriptor(BadFileDescriptor)
    , BrokenPipe(BrokenPipe)
    , CallInterupted(CallInterupted)
    , ConnectionRefused(ConnectionRefused)
    , InputOutput(InputOutput)
    , HostUnreachable(HostUnreachable)
    )
import Data.ByteString (ByteString)
import Data.Int (Int)
import Data.Function ((.), ($))
import Data.Maybe (maybe)
import Data.Streaming.Network (getSocketFamilyTCP)
--import GHC.IO.Exception (IOErrorType(NoSuchThing))
import qualified Network.Socket as Socket
    ( Family
    , SockAddr
    , Socket
    , close
    )
import qualified Network.Socket.ByteString as Socket (send, recv)
import System.IO (IO)

import Prelude (undefined)
import GHC.IO.Exception (ioe_errno)

getTCPSocketAddress
    :: ByteString
    -> Int
    -> Socket.Family
    -> IO (Socket.Socket, Socket.SockAddr)
getTCPSocketAddress = (((mapper `handle`) .) .) . getSocketFamilyTCP
  where
    mapper e = maybe (handleNoErrno e) handleByErrno (ioe_errno e)
      where
        handleNoErrno = undefined
        handleByErrno = \case
            111 -> throw $ ConnectionRefused e
            113 -> throw $ HostUnreachable e
            _ -> undefined

closeSocket
    :: Socket.Socket
    -> IO ()
closeSocket = (mapper `handle`) . Socket.close
  where
    mapper e = maybe (handleNoErrno e) handleByErrno (ioe_errno e)
      where
        handleNoErrno = undefined
        handleByErrno = \case
            4 -> throw $ CallInterupted e
            5 -> throw $ InputOutput e
            9 -> throw $ BadFileDescriptor e
            _ -> undefined

send
    :: Socket.Socket
    -> ByteString
    -> IO Int
send = ((mapper `handle`) .) . Socket.send
  where
    mapper e = maybe (handleNoErrno e) handleByErrno (ioe_errno e)
      where
        handleNoErrno = undefined
        handleByErrno = \case
            32 -> throw $ BrokenPipe e
            _ -> undefined

recv
    :: Socket.Socket
    -> Int
    -> IO ByteString
recv = ((mapper `handle`) .) . Socket.recv
  where
    mapper e = maybe (handleNoErrno e) handleByErrno (ioe_errno e)
      where
        handleNoErrno = undefined
        handleByErrno = \case
            32 -> throw $ BrokenPipe e
            _ -> undefined

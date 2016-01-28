{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Applicative ((<$>))
import Control.Exception (mapException)
import Data.ByteString (ByteString)
import Data.Int (Int)
import Data.Function ((.))
import Data.Streaming.Network (getSocketFamilyTCP)
import GHC.IO.Exception (IOErrorType(NoSuchThing))
import qualified Network.Socket as Socket
    ( Family
    , Socket
    , SockAddr
    )
import System.IO (IO)
import System.IO.Error (IOError, ioeGetErrorType)

import Network.Connections.Internal.Types.Exception
    (ConnectionRefused(ConnectionRefused)
    , ConnectionRefusedException
    )

getTCPSocketAddress
    :: ByteString
    -> Int
    -> Socket.Family
    -> IO (Socket.Socket, Socket.SockAddr)
getTCPSocketAddress = (((mapException mapper <$>) .) .) getSocketFamilyTCP
    --(\e -> print ("error is : " ++ (show $ ioeGetErrorType e)) >> throw e)
  where
    mapper :: IOError -> ConnectionRefusedException
    mapper e = case ioeGetErrorType e of
        NoSuchThing -> ConnectionRefused e

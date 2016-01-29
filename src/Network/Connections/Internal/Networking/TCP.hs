{-# LANGUAGE NoImplicitPrelude #-}
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
import Control.Exception ({-mapException, -}throw, catch)
import Data.ByteString (ByteString)
import Data.Int (Int)
import Data.Function ((.), ($), flip)
import Data.Streaming.Network (getSocketFamilyTCP)
import GHC.IO.Exception (IOErrorType(NoSuchThing))
import qualified Network.Socket as Socket
    ( Family
    , SockAddr
    , Socket
    )
import System.IO (IO)
import System.IO.Error (IOError, ioeGetErrorType, ioeGetErrorString)

import Network.Connections.Internal.Types.Exception
    ( ConnectionRefused(ConnectionRefused)
    , ConnectionRefusedException
    , NoRouteToHost(NoRouteToHost)
    , NoRouteToHostException
    , type (:^:)(E1, E2)
    )

getTCPSocketAddress
    :: ByteString
    -> Int
    -> Socket.Family
    -> IO (Socket.Socket, Socket.SockAddr)
--getTCPSocketAddress = (((mapException mapper <$>) .) .) getSocketFamilyTCP
getTCPSocketAddress = ((((throw . mapper) `rcatch`) .) .) . getSocketFamilyTCP
  where
    rcatch = flip catch
    --mapper :: IOError -> NoRouteToHostException :^: ConnectionRefusedException
    mapper :: IOError -> ConnectionRefusedException
    mapper e = case ioeGetErrorType e of
        NoSuchThing -> case ioeGetErrorString e of
            --"No route to host" -> NoRouteToHost e
            "Connection refused" -> ConnectionRefused e
            _ -> undefined
        _ -> undefined
    undefined = undefined
    --mapper e = case ioeGetErrorType e of
    --    NoSuchThing -> case ioeGetErrorString e of
    --        "No route to host" -> E1 $ NoRouteToHost e
    --        "Connection refused" -> E2 $ ConnectionRefused e
    --        _ -> undefined
    --    _ -> undefined
    --undefined = undefined

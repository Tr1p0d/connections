{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------
-- |
-- Module      :  The Connections library interface
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This file contains the library interface trough which
-- it can be used.
--------------------------------------------------------------------
module Network.Connections where

import Control.Applicative ((<*))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Function (($))
import Data.Proxy (Proxy)

import Network.Connections.Class.Connection
    ( Connection
    , ConnectionSettings
    , OnCloseErrorHandler
    , OnConnectErrorHandler
    , RecvError
    , SendError
    , closeConnection
    , establishConnection
    , recvData
    , sendData
    )
import Network.Connections.Types.ConnectionData
    ( ConnectionData
    , mkConnectionData
    )

runClient ::
  ( Connection c
  , MonadCatch m
  , MonadIO m
  )
  => Proxy c
  -> ConnectionSettings c
  -> OnConnectErrorHandler c m
  -> OnCloseErrorHandler c m
  -> (ConnectionData m (SendError c) (RecvError c) -> m a)
  -> m a
runClient p settings connectHandler closeHandler app = do
    accessConn <- connect
    app (mkData accessConn) <* close accessConn
  where
    connect = connectHandler $ establishConnection p settings
    close accessConn = closeHandler $ closeConnection p accessConn
    mkData accessConn = mkConnectionData
        (sendData p accessConn)
        (recvData p accessConn)

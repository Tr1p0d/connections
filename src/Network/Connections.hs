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
import Control.Exception (Exception)
import qualified Control.Monad.TaggedException as E (catch)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Function (($))

import Network.Connections.Class.Connection
  ( Connection
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
import Network.Connections.Types.ConnectionData (ConnectionData(ConnectionData))

type OnConnectErrorHandler c m =
    EstablishConnectionError c -> m (ConnectionAccessor c)
type OnCloseErrorHandler c m = CloseConnectionError c -> m ()

runClient ::
  ( Connection c
  , MonadIO m
  , MonadCatch m
  , Exception (EstablishConnectionError c)
  , Exception (CloseConnectionError c)
  )
  => c
  -> OnConnectErrorHandler c m
  -> OnCloseErrorHandler c m
  -> (ConnectionData m (SendError c) (RecvError c) -> m a)
  -> m a
runClient connection connectHandler closeHandler app = do
    accessConn <- connect
    app (mkData accessConn) <* close accessConn
  where
    connect = E.catch (establishConnection connection) connectHandler
    close accessConn = E.catch
        (closeConnection connection accessConn)
        closeHandler
    mkData accessConn = ConnectionData
        (sendData connection accessConn)
        (recvData connection accessConn)

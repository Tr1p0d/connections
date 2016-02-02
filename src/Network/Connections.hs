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
import qualified Control.Monad.TaggedException.Internal.Throws as E
    (hideException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy (Proxy)

import Network.Connections.Class.Connection
  ( Connection
  , CloseConnectionError
  , ConnectionAccessor
  , ConnectionSettings
  , EstablishConnectionError
  , RecvError
  , SendError
  , closeConnection
  , establishConnection
  , recvData
  , sendData
  )
import Network.Connections.Internal.Types.Exception (catch')
import Network.Connections.Types.ConnectionData
  ( ConnectionData
  , mkConnectionData
  )

--type OnConnectErrorHandler c m =
--    EstablishConnectionError c -> m (ConnectionAccessor c)
--type OnCloseErrorHandler c m = CloseConnectionError c -> m ()

runClient ::
  ( Connection c
  , MonadCatch m
  , MonadIO m
  )
  => Proxy c
  -> ConnectionSettings c
 -- -> OnConnectErrorHandler c m
 -- -> OnCloseErrorHandler c m
  -> (ConnectionData m (SendError c) (RecvError c) -> m a)
  -> m a
runClient p settings {-connectHandler closeHandler-} app = do
    accessConn <- connect
    app (mkData accessConn) <* close accessConn
  where
    connect = E.hideException (establishConnection p settings) --connectHandler
    close accessConn = E.hideException
        (closeConnection p accessConn)
        --closeHandler
    mkData accessConn = mkConnectionData
        (sendData p accessConn)
        (recvData p accessConn)
    connectHandler = undefined
    closeHandler = undefined
    undefined = undefined

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------
-- |
-- Module      :  The Connection type class
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This is the Connection type class.
--------------------------------------------------------------------
module Network.Connections.Class.Connection where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.TaggedException (Throws)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy)

class Connection c where
    type ConnectionAccessor c :: *
    type ConnectionSettings c :: *

    type EstablishConnectionError c :: [*]
    establishConnection
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> ConnectionSettings c
        -> Throws (EstablishConnectionError c) m (ConnectionAccessor c)

    type CloseConnectionError c
    closeConnection
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> ConnectionAccessor c
        -> Throws (CloseConnectionError c) m ()

    type SendError c
    sendData
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> ConnectionAccessor c
        -> ByteString
        -> Throws (SendError c) m ()

    type RecvError c
    recvData
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> ConnectionAccessor c
        -> Throws (RecvError c) m ByteString

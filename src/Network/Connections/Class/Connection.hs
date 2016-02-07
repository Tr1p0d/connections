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

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.TaggedException as E (Throws)
import Data.Proxy (Proxy)

import qualified Network.Connections.Class.Transport as T
    ( ConnectionAccessor
    , ClientSettings
    , Transport
    , ServerSettings
    )

type OnConnectErrorHandler c m
    =  E.Throws (EstablishConnectionError c) m (T.ConnectionAccessor c)
    -> m (T.ConnectionAccessor c)
type OnCloseErrorHandler c m
    = E.Throws (CloseConnectionError c) m ()
    -> m ()

class T.Transport c => ClientConnection c where
    type EstablishConnectionError c :: [*]
    establishConnection
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> T.ClientSettings c
        -> E.Throws (EstablishConnectionError c) m (T.ConnectionAccessor c)

    type CloseConnectionError c :: [*]
    closeConnection
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> T.ConnectionAccessor c
        -> E.Throws (CloseConnectionError c) m ()

class T.Transport c => ServerConnection c where
    type EstablishServerError c :: [*]
    establishServer
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> T.ServerSettings c
        -> E.Throws (EstablishServerError c) m (T.ConnectionAccessor c)

    type CloseServerError c :: [*]
    closeServer
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> T.ConnectionAccessor c
        -> E.Throws (CloseServerError c) m ()

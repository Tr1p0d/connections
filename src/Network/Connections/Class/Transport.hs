{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------
-- |
-- Module      :  The Transport type class
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This class is supposed to hold all the information
-- common to both Server and Client.
--------------------------------------------------------------------
module Network.Connections.Class.Transport where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.TaggedException as E (Throws)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy)

class Transport c where
    type ConnectionAccessor c :: *
    type ServerSettings c :: *
    type ClientSettings c :: *

    type SendError c :: [*]
    sendData
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> ConnectionAccessor c
        -> ByteString
        -> E.Throws (SendError c) m ()

    type RecvError c :: [*]
    recvData
        :: (MonadIO m, MonadThrow m)
        => Proxy c
        -> ConnectionAccessor c
        -> E.Throws (RecvError c) m ByteString

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
-- |
-- Module      :  TCP ServerConnection type class instance
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains an implementation of the ServerConnection
-- instance for the TCP data type.
--------------------------------------------------------------------
module Network.Connections.Instances.TCP.ServerConnection where

import Network.Connections.Class.Connection
    ( CloseServerError
    , EstablishServerError
    , ServerConnection
    )
import Network.Connections.Types.TCP (TCP)
import Network.Connections.Instances.TCP.Transport ()

instance ServerConnection TCP where
    type EstablishServerError TCP = '[]

    type CloseServerError TCP = '[]

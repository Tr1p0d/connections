{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------
-- |
-- Module      :  The TCP Transport type
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides the TCP alogside some helper functions.
-- Its intention is to define the TCP transport connection
-- with its settings.
--------------------------------------------------------------------
module Network.Connections.Types.TCP where

import Data.ByteString (ByteString)
import Data.Int (Int)

data TCP

data TCPSettings = TCPSettings
  { _host :: ByteString
  , _port :: Int
  }

mkTCPSettings :: ByteString -> Int -> TCPSettings
mkTCPSettings = TCPSettings

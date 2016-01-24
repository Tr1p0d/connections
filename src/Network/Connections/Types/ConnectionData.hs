{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------
-- |
-- Module      :  The ConnectionData type
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides the application data type consisting of
-- read and write functions parametrized over their exceptions and
-- underlying monad.
--------------------------------------------------------------------
module Network.Connections.Types.ConnectionData where

import Control.Monad.TaggedException (Throws)
import Data.ByteString (ByteString)

data ConnectionData m es er = ConnectionData
  { send ::  ByteString -> Throws es m ()
  , recv ::  Throws er m ByteString
  }

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Network.Connections.Types.ConnectionData
  ( ConnectionData
  , mkConnectionData
  , send
  , recv
  )
where

import Control.Lens (makeLenses)
import Control.Monad.TaggedException (Throws)
import Data.ByteString (ByteString)

type Send es m = ByteString -> Throws es m ()
type Recv er m = Throws er m ByteString

data ConnectionData m es er = ConnectionData
  { _send ::  Send es m
  , _recv ::  Recv er m
  }

mkConnectionData :: Send es m -> Recv er m -> ConnectionData m es er
mkConnectionData = ConnectionData

makeLenses ''ConnectionData

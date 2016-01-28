{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------
-- |
-- Module      :  IOException wrappers
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides wrappers for IOException to make
-- Thows signatures more readable.
--------------------------------------------------------------------
module Network.Connections.Internal.Types.Exception where

import Control.Exception (Exception)
import System.IO.Error (IOError)
import Text.Show (Show)

newtype ConnectionRefused a = ConnectionRefused a
  deriving (Show)
type ConnectionRefusedException = ConnectionRefused IOError

instance Exception (ConnectionRefused IOError)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

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
import Data.Typeable (Typeable)
import System.IO.Error (IOError)
import Text.Show (Show)

newtype ConnectionRefused e = ConnectionRefused e
  deriving (Show, Typeable)

instance Exception (ConnectionRefused IOError)

newtype NoRouteToHost e = NoRouteToHost e
  deriving (Show, Typeable)

instance Exception (NoRouteToHost IOError)

type ConnectionRefusedException = ConnectionRefused IOError
type NoRouteToHostException = NoRouteToHost IOError

data e1 :^: e2
    = E1 e1
    | E2 e2
  deriving (Show, Typeable)

instance Exception (ConnectionRefusedException :^: NoRouteToHostException)
instance Exception (NoRouteToHostException:^: ConnectionRefusedException)

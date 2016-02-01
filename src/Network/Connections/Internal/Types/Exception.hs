{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Control.Exception.Errno
    ( ConnectionRefusedError
    , HostUnreachableError
    )
import Control.Monad (Monad, return)
import qualified Control.Monad.TaggedException as E (Throws)
import Data.Typeable (Typeable)
import Text.Show (Show)

import Prelude (undefined)

catch
    :: (e -> m a)
    -> (E.Throws es m a)
    -> CatchesResult (E.Throws (e `Catch` es) m a)
catch = undefined

computation :: E.Throws '[Exception1] m ()
computation = undefined -- (return ())

handler :: Exception1 -> m ()
handler = undefined

type family CatchesResult t :: * where
    CatchesResult (E.Throws '[] m a) = m a
    CatchesResult a = a

type family e1 `Catch` e2 where
    e1 `Catch` (e1 ': es) = es
    e1 `Catch` (e2 ': '[]) = '[]
    e1 `Catch` (e2 ': es) = e2 ': (e1 `Catch` es)

data Exception1 = Exception1
data Exception2 = Exception2

data e1 :^: e2
    = E1 e1
    | E2 e2
    deriving (Show, Typeable)

instance Exception
    ( HostUnreachableError :^: ConnectionRefusedError
    )

instance Exception
    ( ConnectionRefusedError :^: HostUnreachableError
    )

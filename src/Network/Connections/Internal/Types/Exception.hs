{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad (Monad)
import Control.Monad.Catch (MonadCatch, catch)
import qualified Control.Monad.TaggedException as E (Throws)
import qualified Control.Monad.TaggedException.Internal.Throws as E
    (Throws(Throws))
import Data.Function ((.), flip, id)

import Prelude (undefined)

catch'
    ::
    ( Exception e
    , MonadCatch m
    , FromThrows (E.Throws (e `Catch` es) m a)
    )
    => E.Throws es m a
    -> (e -> m a)
    -> EvalThrows (E.Throws (e `Catch` es) m a)
catch' = (evalThrows .) . catch''
  where
    catch''
        :: (Exception e, MonadCatch m)
        => E.Throws es m a
        -> (e -> m a)
        -> E.Throws (e `Catch` es) m a
    catch'' (E.Throws ma) = E.Throws . catch ma

handle'
    ::
    ( Exception e
    , MonadCatch m
    , FromThrows (E.Throws (e `Catch` es) m a)
    )
    => (e -> m a)
    -> E.Throws es m a
    -> EvalThrows (E.Throws (e `Catch` es) m a)
handle' = flip catch'

class FromThrows a where
    type EvalThrows a :: *

    evalThrows :: a -> EvalThrows a

instance Monad m => FromThrows (E.Throws '[] m a) where
    type EvalThrows (E.Throws '[] m a) = m a

    evalThrows (E.Throws ma) = ma

instance Monad m => FromThrows (E.Throws '[e] m a) where
    type EvalThrows (E.Throws '[e] m a) = E.Throws '[e] m a

    evalThrows = id

instance Monad m => FromThrows (E.Throws '[e, e1] m a) where
    type EvalThrows (E.Throws '[e, e1] m a) = E.Throws '[e, e1] m a

    evalThrows = id

type family (e1 :: *) `Catch` (e2 :: [*]) where
    e1 `Catch` (e1 ': es) = es
    e1 `Catch` '[] = '[]
    e1 `Catch` (e2 ': es) = e2 ': (e1 `Catch` es)

computation :: E.Throws [HostUnreachableError, ConnectionRefusedError] m ()
computation = undefined

handler :: HostUnreachableError -> m ()
handler = undefined

handler' :: ConnectionRefusedError -> m ()
handler' = undefined


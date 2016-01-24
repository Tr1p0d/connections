{-# LANGUAGE NoImplicitPrelude #-}

module Network.Connections.Types.TCP where

import Data.ByteString (ByteString)
import Data.Int (Int)

data TCP = TCP
    { _host :: ByteString
    , _port :: Int
    }

mkTCPClient :: ByteString -> Int -> TCP
mkTCPClient = TCP

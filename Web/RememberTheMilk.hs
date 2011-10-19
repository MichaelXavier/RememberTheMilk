--------------------------------------------------------------------
-- |
-- Module      : Web.RememberTheMilk
-- Description : Toplevel module for the RTM API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.next>
-- Stability : provisional
-- Portability: portable
--
-- Toplevel module for the RTM API operating in the RTMM Monad. 
-- 
--------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Web.RememberTheMilk () where

import Web.RememberTheMilk.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as BS16
import           Data.ByteString (ByteString, append)
import           Data.Serialize (encode)
import           Data.Digest.Pure.MD5 (md5)
import           Data.Text.Encoding (encodeUtf8)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Network.HTTP.Types (Query(..), renderQuery)


signRequest :: Query -> RTMSecret -> ByteString
signRequest params sec = BS16.encode . encode . md5 . LBS.fromChunks $ secBS:sortedParams
  where sortedParams         = map concatQ $ sortBy (comparing fst) params
        secBS                = encodeUtf8 sec
        concatQ (k, Just v)  = k `BS.append` v
        concatQ (k, Nothing) = k

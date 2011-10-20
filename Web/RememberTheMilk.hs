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

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT) --DEBUGGING
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as BS16
import           Data.ByteString (ByteString, append)
import           Data.Serialize (encode)
import           Data.Digest.Pure.MD5 (md5)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Network.HTTP.Enumerator
import           Network.HTTP.Types (Query(..), renderQuery)

---- Helpers

doGet :: Query 
         -> RTMSecret
         -> RTMAuth
         -> Text
         -> RTMM (Int, LBS.ByteString)
doGet qs sec auth meth = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
  where req = genRequest qs sec auth meth

--TODO: probably take RTMAuth instead of piecemeal
genRequest :: Query
              -> RTMSecret
              -> RTMAuth
              -> Text
              -> Request m
genRequest qs sec auth meth = def { host        = h,
                                    path        = pth,
                                    port        = 443,
                                    secure      = True,
                                    queryString = qs' }
  where h   = "api.rememberthemilk.com"
        pth = "/services/rest/"
        qs' = finalizeQuery qs sec auth meth

finalizeQuery :: Query
                 -> RTMSecret
                 -> RTMAuth
                 -> Text
                 -> Query
finalizeQuery qs sec auth meth = fmtQ:keyQ:methQ:qs
  where methQ = ("method", Just $ encodeUtf8 meth)
        fmtQ  = ("format", Just "json")
        keyQ  = case auth of
                  APIKey key    -> ("api_key", Just $ encodeUtf8 key)
                  AuthToken tok -> ("auth_token", Just $ encodeUtf8 tok)

signQuery :: Query
             -> RTMSecret
             -> Query
signQuery qs sec = sigQ:qs
  where sigQ = ("api_sig", Just $ genSignature qs sec)

genSignature:: Query
               -> RTMSecret
               -> ByteString
genSignature params sec = BS16.encode . encode . md5 . LBS.fromChunks $ secBS:sortedParams
  where sortedParams         = map concatQ $ sortBy (comparing fst) params
        secBS                = encodeUtf8 sec
        concatQ (k, Just v)  = k `BS.append` v
        concatQ (k, Nothing) = k

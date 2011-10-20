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
module Web.RememberTheMilk (addContact,
                            deleteContact,
                            enumContacts,
                            getContacts,
                            doGet -- DEBUGGING
                            ) where

import Web.RememberTheMilk.Monad

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
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


---- Contacts

addContact = undefined
deleteContact = undefined
enumContacts = undefined
getContacts = undefined

---- Helpers

doGet :: Query 
         -> RTMEnv
         -> Text
         -> RTMM (Int, LBS.ByteString)
doGet qs env meth = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
  where req = genRequest qs env meth

--TODO: probably take RTMAuth instead of piecemeal
genRequest :: Query
              -> RTMEnv
              -> Text
              -> Request m
genRequest qs env meth = def { host        = h,
                               path        = pth,
                               port        = 443,
                               secure      = True,
                               queryString = qs' }
  where h   = "api.rememberthemilk.com"
        pth = "/services/rest/"
        qs' = finalizeQuery qs env meth

finalizeQuery :: Query
                 -> RTMEnv
                 -> Text
                 -> Query
finalizeQuery qs RTMEnv { rtmKey    = key,
                          rtmToken  = tok,
                          rtmSecret = sec} meth = signQuery sec $ fmtQ:keyQ:tokQ:methQ:qs
  where methQ = ("method", Just $ encodeUtf8 meth)
        fmtQ  = ("format", Just "json")
        keyQ  = ("api_key", Just $ encodeUtf8 key)
        tokQ  = ("auth_token", Just $ encodeUtf8 tok)

signQuery :: RTMSecret
             -> Query
             -> Query
signQuery sec qs = sigQ:qs
  where sigQ = ("api_sig", Just $ genSignature qs sec)

genSignature:: Query
               -> RTMSecret
               -> ByteString
genSignature params sec = BS16.encode . encode . md5 . LBS.fromChunks $ secBS:sortedParams
  where sortedParams         = map concatQ $ sortBy (comparing fst) params
        secBS                = encodeUtf8 sec
        concatQ (k, Just v)  = k `BS.append` v
        concatQ (k, Nothing) = k

withEnv :: (RTMEnv -> RTMM a)
           -> RTMM a
withEnv fn = fn =<< ask

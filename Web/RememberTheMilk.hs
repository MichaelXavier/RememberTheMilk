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
                            getContacts,
                            getGroups,
                            getFrob,
                            getToken,
                            doGet,--DEBUG
                            checkToken,
                            genAuthUrl) where

import Web.RememberTheMilk.Monad
import Web.RememberTheMilk.Types
import Web.RememberTheMilk.ParseHelpers

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import           Data.Aeson (json,
                             FromJSON,
                             fromJSON,
                             parseJSON,
                             Result(..),
                             (.:),
                             Value(Object, String))
import           Data.Aeson.Types (typeMismatch)
import           Data.Attoparsec.Lazy (parse, eitherResult)
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as BS16
import           Data.ByteString (ByteString, append)
import           Data.Serialize (encode)
import           Data.Digest.Pure.MD5 (md5)
import           Data.Text (Text, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import qualified Network.URL as U
import           Network.HTTP.Enumerator
import           Network.HTTP.Types (Query(..), renderQuery)

---- Contacts

addContact = undefined
deleteContact = undefined

getContacts :: RTMM (RTMResponse [User])
getContacts = return . fmap unContacts =<< genericGet [] "rtm.contacts.getList"

---- Groups
getGroups :: RTMM (RTMResponse [Group])
getGroups = return . fmap unGroups =<< genericGet [] "rtm.groups.getList"

---- Authentication

getFrob :: RTMM (RTMResponse Frob)
getFrob = genericGet [] "rtm.auth.getFrob"

getToken :: Frob -> RTMM (RTMResponse RTMTokenSummary)
getToken (Frob frob) = genericGet params "rtm.auth.getToken"
  where params = [("frob", Just $ encodeUtf8 frob)]

checkToken :: RTMM (RTMResponse RTMTokenSummary)
checkToken = genericGet [] "rtm.auth.checkToken"

genAuthUrl :: Frob
              -> RTMSecret
              -> RTMKey
              -> Permissions
              -> U.URL
genAuthUrl frob sec key perms = U.URL { U.url_type   = ut,
                                        U.url_path   = up,
                                        U.url_params = params }
  where ut             = U.Absolute hst
        up             = "services/auth/"
        hst            = U.Host { U.protocol = (U.HTTP True),
                                  U.host     = "www.rememberthemilk.com",
                                  U.port     = Nothing }
        params         = map convert $ signQuery sec [keyQ, permQ, frobQ frob]
        keyQ           = ("api_key", Just $ encodeUtf8 key)
        permQ          = ("perms", Just $ permVal perms)
        frobQ (Frob f) = ("frob", Just $ encodeUtf8 f)
        permVal Read   = "read"
        permVal Write  = "write"
        permVal Delete = "delete"
        convert (k, v) = (unpack k, maybe "" unpack v)

---- Helpers

genericGet :: FromJSON a => Query
                            -> Text
                            -> RTMM (RTMResponse a)
genericGet qs meth = withEnv $ \env -> return . handleResponse =<< doGet qs env meth

handleResponse :: FromJSON a => (Int, LBS.ByteString)
                                -> RTMResponse a
handleResponse (200, str) = either parseFail fjson parsed
  where fjson v = case fromJSON v of
                    Success a -> a
                    Error e   -> Left $ RTMFail 0 $ pack e
        parseFail err = Left $ RTMFail 0 $ pack err
        parsed  = eitherResult $ parse json str
handleResponse (_, str) = Left $ RTMFail 0 $ decodeUtf8 . BS.concat . LBS.toChunks $ str

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

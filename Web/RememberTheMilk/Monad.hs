--------------------------------------------------------------------
-- |
-- Module      : Web.RememberTheMilk.Monad
-- Description : Monadic interface for communcating with the RTM API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Web.RememberTheMilk.Monad where

import Data.Text (Text,unpack)
import Control.Monad.Reader

--TODO: get token from frob

data RTMAuth = APIKey Text |  -- ^ Authentication using an API key
               AuthToken Text -- ^ Authenticate using a token obtianed via OAuth V2. Currently no way in the library to obtain refresh tokens

type RTMSecret = Text

-- | Environment passed into requests when they are executed within a RTMM
data RTMEnv = RTMEnv { rtmAuth :: RTMAuth,    -- ^ Preferred RememberTheMilk authentication
                       rtmSecret :: RTMSecret -- ^ Shared secret used for signing requests
                     }

-- | IO wrapper used to compose/sequence RTM API actions. See Web.RTM docs for examples
newtype RTMM a = RTMM {unRTMM :: ReaderT RTMEnv IO a} 
  deriving (Monad, MonadIO, MonadReader RTMEnv)

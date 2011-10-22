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

import Web.RememberTheMilk.Types

import Data.Text (Text,unpack)
import Control.Monad.Reader

-- | IO wrapper used to compose/sequence RTM API actions. See Web.RTM docs for examples
newtype RTMM a = RTMM {unRTMM :: ReaderT RTMEnv IO a} 
  deriving (Monad, MonadIO, MonadReader RTMEnv)

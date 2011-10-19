--------------------------------------------------------------------
-- |
-- Module      : Web.RememberTheMilk.Types
-- Description : Types returned by the RTM API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
module Web.GooglePlus.Types () where

import           Control.Applicative ((<$>), (<*>), pure, Applicative)
import           Data.Aeson (Value(..),
                             Object,
                             FromJSON,
                             parseJSON,
                             (.:),
                             (.:?))
import           Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Map as M
import           Data.Text (Text, unpack)
import qualified Data.Text as T

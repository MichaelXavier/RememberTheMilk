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

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module Web.RememberTheMilk.Types (Frob(..),
                                  RTMFail(..),
                                  RTMResponse(..)) where

import           Control.Applicative ((<$>), (<*>), pure, Applicative)
import           Data.Aeson (Value(..),
                             Object,
                             FromJSON,
                             parseJSON,
                             (.:),
                             (.:?))
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Text (Text, unpack)
import qualified Data.Text as T

data Frob = Frob Text deriving (Show, Eq)

instance FromJSON Frob where
  parseJSON (Object v) = Frob <$> v .:/ ["rsp", "frob"]
  parseJSON v          = typeMismatch "Frob" v

type Code = Int

type RTMResponse a = Either RTMFail a

data RTMFail = RTMFail Code Text deriving (Show, Eq)

instance FromJSON a => FromJSON (RTMResponse a) where
  parseJSON obj@(Object v) = parseStat $ obj `dv` ["rsp", "stat"]
    where parseStat (Just (String "fail")) = Left <$> parsedFail
          parseStat (Just (String "ok"))   = Right <$> parseJSON obj
          parseStat (Just stat)            = typeMismatch "Response stat" stat
          parseStat Nothing                = fail "Could not find rsp/stat"
          parsedFail                       = RTMFail <$> v .:/ ["rsp", "stat", "code"]
                                                     <*> v .:/ ["rsp", "stat", "msg"]
  parseJSON v              = typeMismatch "RTMResponse" v


---- Helpers
(.:/) :: (FromJSON a) => Object
                         -> [Text]
                         -> Parser a
obj .:/ (k:ks) = maybe (fail msg) parseJSON parsed
 where parsed = deepValue ks =<< M.lookup k obj
       msg    = "Failed to find " ++ (intercalate "/" $ map unpack (k:ks))
obj .:/ []     = parseJSON $ Object obj

deepValue :: [Text]
             -> Value
             -> Maybe Value
deepValue (k:[]) (Object obj) = M.lookup k obj
deepValue (k:[]) _            = Nothing
deepValue (k:ks) (Object obj) = deepValue ks =<< M.lookup k obj
deepValue (k:ks) _            = Nothing
deepValue [] v                = Just v

dv = flip deepValue

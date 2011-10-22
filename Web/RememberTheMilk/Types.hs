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

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Web.RememberTheMilk.Types (Frob(..),
                                  User(..),
                                  Group(..),
                                  Contacts(..),
                                  Groups(..),
                                  Permissions(..),
                                  RTMTokenSummary(..),
                                  RTMFail(..),
                                  RTMResponse(..),
                                  RTMSecret,
                                  RTMKey,
                                  RTMToken,
                                  RTMEnv(..)) where

import Web.RememberTheMilk.ParseHelpers

import           Control.Applicative ((<$>), (<*>), pure)
import           Data.Aeson (Value(..),
                             Object,
                             FromJSON,
                             parseJSON,
                             (.:),
                             (.:?))
import           Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Map as M
import           Data.Text (Text, unpack)

data Frob = Frob Text deriving (Show, Eq)

instance FromJSON Frob where
  parseJSON (Object v) = Frob <$> v .:/ ["rsp", "frob"]
  parseJSON v          = typeMismatch "Frob" v

type ID = Text

type Code = Int

type RTMResponse a = Either RTMFail a

data RTMFail = RTMFail Code Text deriving (Show, Eq)

data Permissions = Read | Write | Delete deriving (Show, Eq)

instance FromJSON Permissions where
  parseJSON (String "read")   = pure Read
  parseJSON (String "write")  = pure Write
  parseJSON (String "delete") = pure Delete
  parseJSON v                 = typeMismatch "Permissions" v

--Sweet mother this is nasty
instance FromJSON a => FromJSON (RTMResponse a) where
  parseJSON obj@(Object v) = objLookup "rsp" parseRsp obj
    where parseRsp root = objLookup "stat" parseStat root
          parseStat (String "fail") = Left <$> parsedFail
          parseStat (String "ok")   = Right <$> v .: "rsp" --TODO: need to figure out how to get the path in at this part
          parseStat stat            = typeMismatch "Response stat" stat
          parsedFail                       = RTMFail <$> v .:/ ["rsp", "err", "code"]
                                                     <*> v .:/ ["rsp", "err", "msg"]
          objLookup key fn (Object val) = maybe (fail $ unpack key ++ " not found") fn $ M.lookup key val
          objLookup key _ _ = fail $ unpack key ++ " not found in non-object"
  parseJSON v              = typeMismatch "RTMResponse" v


data RTMTokenSummary = RTMTokenSummary { toksumToken :: RTMToken,
                                         toksumPerms :: Permissions,
                                         toksumUser  :: User } deriving (Show, Eq)

instance FromJSON RTMTokenSummary where
  parseJSON (Object v) = RTMTokenSummary <$> v .: "token"
                                         <*> v .: "perms"
                                         <*> v .: "user"
  parseJSON v          = typeMismatch "RTMTokenSummary" v


data User = User { userId       :: ID,
                   userFullname :: Maybe Text,
                   userName     :: Maybe Text
                 } deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object v) = User <$> v .: "id"
                              <*> v .:? "fullname"
                              <*> v .:? "username"
  parseJSON v          = typeMismatch "RTMTokenSummary" v

data Group = Group { groupId       :: ID,
                     groupName     :: Text,
                     groupContacts :: [User] } deriving (Show, Eq)

instance FromJSON Group where
  parseJSON obj@(Object v) = Group <$> v .: "id"
                                         <*> v .: "name"
                                         <*> (unContacts `fmap` parseJSON obj)
  parseJSON v          = typeMismatch "Group" v

type RTMSecret = Text
type RTMKey    = Text
type RTMToken   = Text

-- | Environment passed into requests when they are executed within a RTMM
data RTMEnv = RTMEnv { rtmKey    :: RTMKey,    -- ^ API key
                       rtmToken  :: Text,   -- ^ Authorization token
                       rtmSecret :: RTMSecret -- ^ Shared secret used for signing requests
                     }

---- Internals, have to export to avoid circular dependency
data Contacts = Contacts { unContacts :: [User]}

instance FromJSON Contacts where
  parseJSON (Object v)    = Contacts <$> v `listAttribute` ["contacts", "contact"]
  parseJSON v             = typeMismatch "Contacts" v

data Groups = Groups { unGroups :: [Group]}

instance FromJSON Groups where
  parseJSON (Object v)    = Groups <$> v `listAttribute` ["groups", "group"]
  parseJSON v             = typeMismatch "Groups" v

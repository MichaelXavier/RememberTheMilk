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
                                  Tasks(..),
                                  TaskList(..),
                                  TaskSeries(..),
                                  Task(..),
                                  Permissions(..),
                                  Location(..),
                                  Filter(..),
                                  Priority(..),
                                  ID,
                                  RTMTokenSummary(..),
                                  RTMFail(..),
                                  RTMResponse(..),
                                  RTMSecret,
                                  RTMKey,
                                  RTMToken,
                                  RTMEnv(..)) where

import Web.RememberTheMilk.ParseHelpers

import           Control.Applicative (Applicative, (<$>), (<*>), pure)
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
import           Data.Time.Calendar.OrdinalDate --TODO: 8601
import           Data.Time.LocalTime (ZonedTime(..), zonedTimeToUTC)
import           Data.Time.RFC3339 (readRFC3339)
import           Network.URL (URL(..), importURL)

data Frob = Frob Text deriving (Show, Eq)

instance FromJSON Frob where
  parseJSON (Object v) = Frob <$> v .:/ ["rsp", "frob"]
  parseJSON v          = typeMismatch "Frob" v

type ID = Text

type Code = Int

type RTMResponse a = Either RTMFail a

data RTMFail = RTMFail Code Text deriving (Show, Eq)

data Permissions = Read |
                   Write |
                   Delete deriving (Show, Eq)

instance FromJSON Permissions where
  parseJSON (String "read")   = pure Read
  parseJSON (String "write")  = pure Write
  parseJSON (String "delete") = pure Delete
  parseJSON v                 = typeMismatch "Permissions" v

--Sweet mother this is nasty
instance FromJSON a => FromJSON (RTMResponse a) where
  parseJSON obj@(Object v) = objLookup "rsp" parseRsp obj
    where parseRsp root = objLookup "stat" parseStat root
          parseStat (String "fail")     = Left <$> parsedFail
          parseStat (String "ok")       = Right <$> v .: "rsp" --TODO: need to figure out how to get the path in at this part
          parseStat stat                = typeMismatch "Response stat" stat
          parsedFail                    = RTMFail <$> v .:/ ["rsp", "err", "code"]
                                                  <*> v .:/ ["rsp", "err", "msg"]
          objLookup key fn (Object val) = maybe (fail $ unpack key ++ " not found") fn $ M.lookup key val
          objLookup key _ _             = fail $ unpack key ++ " not found in non-object"
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
type RTMToken  = Text

-- Abstracting away taskseries

data TaskList = TaskList { taskListId      :: ID,
                           taskListCureent :: ZonedTime, -- ^ Not documented
                           taskListSeries  :: [TaskSeries] } deriving (Show, Eq)

instance FromJSON TaskList where
  parseJSON (Object v) = TaskList <$> v .: "id"
                                  <*> v .: "current"
                                  <*> v .: "series"
  parseJSON v          = typeMismatch "TaskList" v

--TODO: participants
--TODO: figure out how to parse recurrence rules
data TaskSeries = TaskSeries { taskSeriesId      :: ID,
                               taskSeriesCreated :: ZonedTime,
                               taskSeriesUpdated :: ZonedTime,
                               taskSeriesName    :: Text,
                               taskSeriesSource  :: Text, -- TODO: get a list of possible values?
                               taskLocationId    :: Maybe ID,
                               taskUrl           :: Maybe URL,
                               taskSeriesTags    :: [Text],
                               taskSeriesNotes   :: [Note],
                               taskSeriesTasks   :: [Task]
                             } deriving (Show, Eq)

instance FromJSON TaskSeries where
  parseJSON (Object v) = TaskSeries <$> v .: "id"
                                    <*> v .: "created"
                                    <*> v .: "updated"
                                    <*> v .: "name"
                                    <*> v .: "source"
                                    <*> v `coerceEmpty` "location_id"
                                    <*> v `coerceEmpty` "url"
                                    <*> v `listAttribute` ["tags", "tag"]
                                    <*> v .: "notes"
                                    <*> v .: "tasks" -- TODO: it has either task or tasks key?
  parseJSON v          = typeMismatch "TaskSeries" v

data Task = Task { taskId        :: ID,
                   taskDue       :: Maybe ZonedTime,
                   taskAdded     :: ZonedTime,
                   taskCompleted :: Maybe ZonedTime,
                   taskDeleted   :: Maybe ZonedTime,
                   taskPriority  :: Priority,
                   taskPostponed :: Integer
                 } deriving (Show, Eq)

instance FromJSON Task where
  parseJSON (Object v) = Task <$> v .:  "id"
                              <*> parsedDue
                              <*> v .:  "added"
                              <*> v .:  "completed"
                              <*> v .:  "deleted"
                              <*> v .:  "priority"
                              <*> v `coerceInteger` "postponed"
    where parsedDue = case M.lookup "has_due_time" v of
                            Just (String "1") -> v .: "due"
                            _                 -> pure Nothing
  parseJSON v          = typeMismatch "Task" v

data Priority = Priority1 |
                Priority2 |
                Priority3 |
                NoPriority deriving (Eq)

instance Show Priority where
  show Priority1  = "1"
  show Priority2  = "2"
  show Priority3  = "3"
  show NoPriority = "N"

instance FromJSON Priority where
  parseJSON (String "1") = pure Priority1
  parseJSON (String "2") = pure Priority2
  parseJSON (String "3") = pure Priority3
  parseJSON (String "N") = pure NoPriority
  parseJSON v            = typeMismatch "Priority" v

data Note = Note { noteId       :: ID,
                   noteCreated  :: ZonedTime,
                   noteModified :: ZonedTime,
                   noteTitle    :: Maybe Text,
                   noteContents :: Text } deriving (Show, Eq)

instance FromJSON Note where
  parseJSON (Object v) = Note <$> v .: "id"
                              <*> v .: "created"
                              <*> v .: "modified"
                              <*> v .: "title"
                              <*> v .: "$t"
  parseJSON v = typeMismatch "Note" v

data Location = Location { locationId :: ID,
                           locationName :: Text,
                           locationLongitude :: Double,
                           locationLatitude :: Double,
                           locationViewable :: Bool,
                           locationZoom :: Integer
                         } deriving (Show, Eq)

instance FromJSON Location where
  parseJSON (Object v) = Location <$> v .: "id"
                                  <*> v .: "name"
                                  <*> v .: "longitude"
                                  <*> v .: "latitude"
                                  <*> v `coerceBool` "viewable"
                                  <*> v `coerceInteger` "zoom"
  parseJSON v = typeMismatch "Location" v

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

data Tasks = Tasks { unTasks :: [Task]}

instance FromJSON Tasks where
  parseJSON (Object v)    = Tasks <$> v `listAttribute` ["tasks", "task"]
  parseJSON v             = typeMismatch "Tasks" v

data Filter = FList Text | -- ^ Specify list name
              FPriority Priority      | -- ^ Specify priority
              FStatus CompletionState | -- ^ Specify task completion status
              FTag Text               | -- ^ Specify exact tag
              FTagContains Text       | -- ^ Specify partial tag
              FIsTagged Bool          | -- ^ Specify whether task is tagged or not
              FLocation Text          | -- ^ Specify location name
              -- Skipping locatedWithin as it is only supported on Android/iPhone
              FIsLocated Bool         | -- ^ Specify whether a task has a location
              FIsRepeating Bool       | -- ^ Specify whether a task is repeating
              FName Text              | -- ^ Specify task by name
              FNoteContains Text      | -- ^ Specify partial contents of a note
              FHasNotes Bool          | -- ^ Specify whether a task has notes
              FDue Text               | -- ^ Specify when due, i.e. "tomorrow" or "never"
              FDueBefore Text         | -- ^ Specify tasks due before the given time
              FDueAfter Text          | -- ^ Specify tasks due after the given time
              FDueWithin Text         | -- ^ Specify tasks due within a time range, i.e. "1 week of today"
              FCompleted Text         | -- ^ Specify tasks completed on a given day, i.e. "today" or "last week"
              FCompletedBefore Text   | -- ^ Specify tasks completed before a given date
              FCompletedAfter Text    | -- ^ Specify tasks completed after a given date
              FCompletedWithin Text   | -- ^ Specify tasks completed within a time range, i.e. "1 week of today"
              FAdded Text             | -- ^ Specify task added on a given day, i.e. "today"
              FAddedBefore Text       | -- ^ Specify tasks added before a given date
              FAddedAfter Text        | -- ^ Specify tasks added after a given date
              FAddedWithin Text       | -- ^ Specify tasks added within a time range, i.e. "1 week of today"
              FTimeEstimate Text      | -- ^ Specify tasks with a given time estimate, i.e. "1 hour" or "> 1 hour"
              FPostponed Text         | -- ^ Specify tasks postponed a number of times, i.e. "3" or "> 3"
              FIsShared Bool          | -- ^ Specify whether task has been shared
              FSharedWith Text        | -- ^ Specify tasks shared with a username
              FIsReceived Bool        | -- ^ Specify tasks that were assigned by someone else
              FTo Text                | -- ^ Specify tasks sent to a given username
              FFrom Text              | -- ^ Specify tasks sent from a given username
              FIncludeArchived Bool   | -- ^ Whether to include archived lists
              FAND [Filter]           | -- ^ Specify a group of filters that must ALL be true
              FOR [Filter]            | -- ^ Specify a group of filters where only 1 needs to be true
              FNOT Filter               -- ^ Negate a filter
              deriving (Eq)

instance Show Filter where
  show (FList lst)           = quoteFilter "list" lst
  show (FPriority p)         = quoteFilter "priority" p
  show (FStatus cs)          = quoteFilter "status" cs --TODO: rederive CompletionState
  show (FTag t)              = quoteFilter "tag" t
  show (FTagContains t)      = quoteFilter "tagContains" t
  show (FIsTagged v)         = quoteFilter "isTagged" v
  show (FLocation l)         = quoteFilter "location" l
  show (FIsLocated il)       = quoteFilter "isLocated"  il
  show (FIsRepeating ir)     = quoteFilter "isRepeating" ir
  show (FName n)             = quoteFilter "name" n
  show (FNoteContains nc)    = quoteFilter "noteContains" nc
  show (FHasNotes hn)        = quoteFilter "hasNotes" hn
  show (FDue d)              = quoteFilter "due" d
  show (FDueBefore db)       = quoteFilter "dueBefore" db
  show (FDueAfter da)        = quoteFilter "dueAfter" da
  show (FDueWithin dw)       = quoteFilter "dueWithin" dw
  show (FCompleted c)        = quoteFilter "completed" c
  show (FCompletedBefore cb) = quoteFilter "completedBefore" cb
  show (FCompletedAfter ca)  = quoteFilter "completedAfter" ca
  show (FCompletedWithin cw) = quoteFilter "completedWithin" cw
  show (FAdded a)            = quoteFilter "added" a
  show (FAddedBefore ab)     = quoteFilter "addedBefore" ab
  show (FAddedAfter aa)      = quoteFilter "addedAfter" aa
  show (FAddedWithin aw)     = quoteFilter "addedWithin" aw
  show (FTimeEstimate te)    = quoteFilter "timeEstimate" te
  show (FPostponed p)        = quoteFilter "postponed" p
  show (FIsShared is)        = quoteFilter "isShared" is
  show (FSharedWith sw)      = quoteFilter "sharedWith" sw
  show (FIsReceived ir)      = quoteFilter "isReceived" ir
  show (FTo t)               = quoteFilter "to" t
  show (FFrom f)             = quoteFilter "from" f
  show (FIncludeArchived ia) = quoteFilter "includeArchived" ia
  show (FAND fs)             = "(" ++ (intercalate "AND" . map show) fs ++ ")"
  show (FOR fs)              = "(" ++ (intercalate "OR" . map show) fs ++ ")"
  show (FNOT f)              = "NOT " ++ show f
  
quoteFilter :: Show a => String -> a -> String
quoteFilter flt v = flt ++ ":" ++ vstr
  where vstr = case show v of
                 "True"  -> "true"
                 "False" -> "false"
                 str     -> str

data CompletionState = Completed |
                       Incomplete deriving (Show, Eq)

---- Helpers
maybeToParser :: (Applicative m, Monad m) => Maybe a
                                             -> String
                                             -> m a
maybeToParser parsed msg = maybe (fail msg) pure parsed

-- Caution: orphan instances
instance Eq ZonedTime where
  a == b = zonedTimeToUTC a == zonedTimeToUTC b

--TODO: doublecheck the format
instance FromJSON ZonedTime where
  parseJSON (String str) = maybeToParser parsed $ "Failed to parse ZonedTime " ++ unpack str
    where parsed = readRFC3339 . unpack $ str
  parseJSON v            = typeMismatch "ZonedTime" v

instance FromJSON URL where
  parseJSON (String str) = maybe (fail $ "Failed to parse URL " ++ unpack str) pure parsed
    where parsed = importURL . unpack $ str
  parseJSON v          = typeMismatch "URL" v
